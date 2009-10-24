;;; collab.el --- peer to peer collaborative editing

;; Copyright (C) 2009 Jonas Reinsch

;; Author: Jonas Reinsch <jonas at collabmode dot org>
;; Keywords: collaboration, collaborative, editing, collab
;; Homepage: http://collabmode.org
;; Version: 0.1

;; collabmode is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; collabmode is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with collabmode.  If not, see <http://www.gnu.org/licenses/>.

(eval-when-compile
  (require 'cl))

(defstruct collab-char id parent undertaker c type)

(defconst collab-in-buffer 0
  "c is an insertable character and not deleted.")
(defconst collab-deleted 1
  "c is an insertable character and deleted.")
(defconst collab-del 2
  "c is the special character DEL (delete)")
(defconst collab-anchor-char 0
  "'Position' of the beginning of the text. If a character is inserted at
position 1, the anchor-char is its parent character.")
(defconst collab-replay-delta 0.03)
(defconst collab-replay-direction 1)
(defconst collab-send-delta (seconds-to-time 0.2))
(defvar collab-send-next nil)

(defvar collab-server-process nil)
(defvar collab-server-port 8000)
(defvar collab-clients nil)
(defvar collab-inhibit-sending nil)
(defvar collab-client-hash nil)

(defvar collab-char-hash nil
  "All characters, even deleted characters and DEL special characters are stored here.
The key is the id of the character.
buffer-local for each buffer running collab.")
(defvar collab-local-pc nil
  "Counter (pc = program counter) that counts all characters issued by this client.
buffer-local for each buffer running collab.")
(defvar collab-before-change-start nil
  "buffer local")
(defvar collab-subsequent-before-change-calls nil
  "Sometimes before-change is called more than once in one change - this variable makes it
possible to deal with that.
buffer-local for each buffer running collab.")
(defvar collab-deleted-string nil
  "If the user deletes a part of the buffer, this variable stores it before the buffer change.
buffer-local for each buffer running collab.")
(defconst collab-id-buffer-preamble
  "This buffer is created and used by collab - please do not modify or kill.\n"
  "String that appears at the beginning of each collab-id-buffer.")
(defconst collab-id-buffer-offset
  (string-bytes collab-id-buffer-preamble)
  "Number of bytes to ignore at the beginning of a collab-id-buffer.")

(defconst collab-id-width 26)
(defconst collab-char-width (+ 2 (* 2 collab-id-width)))
(defconst collab-pos0-id (make-string collab-id-width ?0))

(defvar collab-next-id nil
  "buffer-local for each buffer.")

(defun collab-make-timestamp ()
  (apply 'format "%04x%04x%06d" (current-time)))

(defun collab-make-id (timestamp local-pc)
  (or collab-next-id
      (format "%s%06x%06x"
	      timestamp local-pc
	      (random #x1000000))))

(defun collab-before-change (start end)
  (unless collab-subsequent-before-change-calls
    (setq collab-deleted-string (buffer-substring start end) ;; preserve properties
	  collab-before-change-start start
	  collab-subsequent-before-change-calls t)))

(defun* collab-after-change (start end lll)
  (setq collab-subsequent-before-change-calls nil)
  (let* ((inserted-string (buffer-substring-no-properties start end))
	 (timestamp (collab-make-timestamp)))
    (when (string= collab-deleted-string inserted-string)
      (return-from collab-after-change))
    ;; both deletion and insertion possible in one change (when in 
    ;; overwrite mode)! Therefore two conditionals (unless/when), not one
    ;; like if/cond (they have "mutually exclusive semantics").
    (unless (zerop lll) ;; deletion
      (loop for deleted-id in (collab-id-buffer-delete-n 
			       ;; important for M-c (capitalize-word) etc.,
			       ;; because the deleted/inserted (capitalized) string
			       ;; might be smaller than reported by before-change
			       ;; example: blubber -> Blubber
			       ;; before-change says that "blubber" is deleted
			       ;; but Emacs 23 only deletes the first b
			       ;; (earlier Emacs versions are different: they delete
			       ;; the whole word and insert the whole (modified) word
			       ;; back in)
			       (substring collab-deleted-string
					  (- start collab-before-change-start)
					  (+ lll (- start collab-before-change-start))))
	    for id = (collab-make-id timestamp (incf collab-local-pc))
	    do
	    (setf (collab-char-type (gethash deleted-id collab-char-hash)) collab-deleted)
	    (puthash
	     id
	     (make-collab-char :id id :parent deleted-id :c "dd" :type collab-del)
	     collab-char-hash)
	    (unless collab-inhibit-sending
	      (collab-become-originator id))))
    (when (< start end) ;; insertion
      (collab-add-insertion-properties start end timestamp)))
  (collab-send-all)) ;; package complete, send it away!

(defun collab-add-insertion-properties (start end timestamp)
  (loop for pos from start below end
	for parent-pos = (1- pos)
	for parent-id = (collab-id-buffer-get-id parent-pos)
	for id = (collab-make-id timestamp (incf collab-local-pc))
	for c = (buffer-substring-no-properties pos (1+ pos))
	do
	  (puthash
	   id
	   (make-collab-char :id id :parent parent-id :c c :type collab-in-buffer)
	   collab-char-hash)
	  (collab-id-buffer-insert pos id)
	  (unless collab-inhibit-sending
	    (collab-become-originator id))))

(defun collab-add-hooks ()
  ;; make hooks buffer-local
  (add-hook 'after-change-functions 'collab-after-change nil t)
  (add-hook 'before-change-functions 'collab-before-change nil t))

(defun collab-remove-hooks ()
  ;; only remove buffer-local hooks
  (remove-hook 'before-change-functions 'collab-before-change t)
  (remove-hook 'after-change-functions 'collab-after-change t))

(defun collab ()
  (interactive)
  (set (make-local-variable 'collab-char-hash) (make-hash-table :test 'equal
								:size 10000))
  (set (make-local-variable 'collab-local-pc) 0)
  (set (make-local-variable 'collab-subsequent-before-change-calls) nil)
  (set (make-local-variable 'collab-deleted-string) nil)
  (set (make-local-variable 'collab-next-id) nil)
  (set (make-local-variable 'collab-inhibit-sending) nil)
  (set (make-local-variable 'collab-client-hash) (make-hash-table :test 'eq))
  (set (make-local-variable 'collab-before-change-start) nil)
  (set (make-local-variable 'collab-send-next) (current-time))
  ;; make the buffer-local variables immune against a major mode change:
  (put 'collab-char-hash 'permanent-local t)
  (put 'collab-local-pc 'permanent-local t)
  (put 'collab-subsequent-before-change-calls 'permanent-local t)
  (put 'collab-before-change-start 'permanent-local t)
  (put 'collab-deleted-string 'permanent-local t)
  (put 'collab-next-id 'permanent-local t)
  (put 'collab-inhibit-sending 'permanent-local t)
  (put 'collab-client-hash 'permanent-local t)
  (put 'collab-send-next 'permanent-local t)

  (collab-add-hooks)
  (let ((saved-buffer-chars-modified-tick (buffer-chars-modified-tick))
	(saved-buffer-modified-p (buffer-modified-p)))
    (collab-add-insertion-properties (point-min) (point-max) (collab-make-timestamp))
    ;; if modified flag of buffer was nil before and only
    ;; text property changes occured then don't treat this as modification
    (if (and (not saved-buffer-modified-p)                       
	     (= saved-buffer-chars-modified-tick (buffer-chars-modified-tick)))
	(set-buffer-modified-p nil)))
;;  (collab-server-start)
;;  (run-at-time t 3 (lambda (b)
;;		     (with-current-buffer b
;;		       (collab-send-all))) (current-buffer))
)

(defun collab-replay ()
  (interactive)
  ;; collab-char-hash (and collab-char-hash-tmp, which is only a reference
  ;; to it) can grow during replay, as new input might arrive
  ;; during sit-for intervals. But the sorting of ids at the
  ;; beginning of this function has the effect of taking a snapshot,
  ;; i.e. all input arriving after snapshotting is not replayed
  (let ((history (sort (loop for k being the hash-key of collab-char-hash collect k) 'string<))
	(collab-char-hash-tmp collab-char-hash))
    (with-current-buffer (generate-new-buffer "collab-replay")
      (with-selected-window (display-buffer (current-buffer))
	(collab)
	(loop with collab-inhibit-sending = t
	      with inhibit-read-only = t ;; paranoia
	      for id in history
	      for char = (gethash id collab-char-hash-tmp)
	      for parent-position = (collab-id-buffer-get-pos (collab-char-parent char))
	      for collab-next-id = id
	      do
	      (sit-for collab-replay-delta)
	      (if (= (collab-char-type char) collab-del) ;; deletion
		  (progn (goto-char parent-position)
			 (delete-char 1))
		(goto-char (1+ parent-position)) ;; insertion
		(insert (collab-char-c char))))))))

(defun collab-replay-fun (replay-buffer collab-char-hash-tmp i h ovl)
  "the name should be changed and get a buffer prefix"
  (when (or (= i (length h)) (= i -1))
    (global-set-key "+" 'self-insert-command)
    (global-set-key "-" 'self-insert-command)
    (global-set-key "<" 'self-insert-command)
    (global-set-key ">" 'self-insert-command)
    (collab-replay-forward)
    (message "%s" i))

  (when (and (>= i 0) (< i (length h)))
    (with-current-buffer replay-buffer
      (save-excursion
	(let* ((id (aref h i))
	       (inhibit-read-only t)
	       (collab-inhibit-sending t)
	       (char (gethash id collab-char-hash-tmp))
	       (parent-position (if (and (= -1 collab-replay-direction)
					 (= (collab-char-type char) collab-del))
				    (progn
				      (collab-id-buffer-get-pos
				       (collab-char-undertaker
					(gethash (collab-char-parent char)
						 collab-char-hash-tmp))))
				  (collab-id-buffer-get-pos (collab-char-parent char))))
	       (collab-next-id id)) 
	                            ;; if backwards:... -> don't set!
	  (when (= 1 collab-replay-direction)
	    (if (= (collab-char-type char) collab-del) ;; deletion
		(progn 
		  (setf (collab-char-undertaker (gethash (collab-char-parent char)
							 collab-char-hash-tmp))
			(collab-id-buffer-get-id (1- parent-position)))
		  (goto-char parent-position)
		  (delete-char 1)
		  (move-overlay ovl parent-position (1+ parent-position)))
	      (goto-char (1+ parent-position)) ;; insertion
	      (insert (collab-char-c char))
	      (move-overlay ovl (+ 2 parent-position) (+ 3 parent-position))))
	  (when (= -1 collab-replay-direction)
	    (let ((inhibit-modification-hooks t))
	      (if (= (collab-char-type char) collab-del)
		  (progn ;; undo deletion: insert parent of del-char after undertaker:-)
		    ;; parent-position holds undertaker-position...
		    (goto-char (1+ parent-position))
		    (insert (propertize
			     (collab-char-c (gethash (collab-char-parent char)
						     collab-char-hash-tmp))
			     'collab-id (collab-char-parent char)))
		    (move-overlay ovl (+ 2 parent-position) (+ 3 parent-position)))
		;; undo insertion
		(goto-char (1+ parent-position))
		(delete-char 1)
		(move-overlay ovl (1+ parent-position) (+ 2 parent-position)))))))))
  (run-at-time collab-replay-delta
	       nil 'collab-replay-fun replay-buffer collab-char-hash-tmp
	       (+ collab-replay-direction i) h ovl))

(defun collab-increase-replay-speed ()
  "sollte buffer-local sein!"
  (interactive)
  (setq collab-replay-delta (/ collab-replay-delta 2)))

(defun collab-decrease-replay-speed ()
  "sollte buffer-local sein!"
  (interactive)
  (setq collab-replay-delta (* collab-replay-delta 2)))

(defun collab-replay-backward ()
  (interactive)
  (setq collab-replay-direction -1)
  (message "collab replay-backward"))

(defun collab-replay-forward ()
  (interactive)
  (setq collab-replay-direction 1)
  (message "collab replay-forward"))

(defun collab-replay-async ()
  (interactive)
  ;; some smaller problems:
  ;; - spaces necessary at the end of a buffer in order to get overlays 
  ;;   displayed correctly, sometimes one of those blanks gets "taken
  ;;   away" when the direction is changed. Understand this better!
  ;; - scrolling only works when positioning point at (point-max).
  ;; - the overlay only displays if the major mode is changed at least once.
  ;; - problems with overlay display if point is at the beginning of a line
  ;;   that only contains a Newline (the Overlay is now stretched over the
  ;;   whole line)
  ;; - there is a mode that displays point of more than one user - how
  ;;   does it work?
  ;; - collab-char-hash (and collab-char-hash-tmp, which is only a reference
  ;;   to it) can grow during replay, as new input might arrive
  ;;   during sit-for intervals. But the sorting of ids at the beginning
  ;;   of this function has the effect of taking a snapshot,
  ;;   i.e. all input arriving after snapshotting is not replayed
  (let* ((history (apply 'vector
			 (sort
			  (loop for k being the hash-key of collab-char-hash collect k) 'string<)))
	 (collab-char-hash-tmp collab-char-hash)
	 (replay-buffer (generate-new-buffer "collab-replay"))
	 (mode major-mode)
	 (ovl (make-overlay 1 2 replay-buffer t)))
    (display-buffer replay-buffer)
    (with-current-buffer replay-buffer
      ;; problems with some modes, e.g. if inserting
      ;; leads to a change of the buffer at another place
      ;; example: ielm
      ;; how to recognize such modes? This is important,
      ;; because otherwise bad things could happen (consider
      ;; two people sharing a shell buffer, which has
      ;; potentially disastrous side effects)
      ;; ielm: buffer-read-only nil, but some parts of the text
      ;; are read-only
      (funcall mode)
      (insert " ")
      (overlay-put ovl 'font-lock-face '(:background "orange"))
      (setq buffer-undo-list t
	    buffer-read-only t)
      (collab))
    (global-set-key "+" 'collab-increase-replay-speed)
    (global-set-key "-" 'collab-decrease-replay-speed)
    (global-set-key ">" 'collab-replay-forward)
    (global-set-key "<" 'collab-replay-backward)
    (collab-replay-fun replay-buffer collab-char-hash-tmp 0 history ovl)))

;; id-buffer
(defun collab-id-buffer-get-pos (id)
  "first looks up the char in collab-char-hash to get the id
that was used when the char was stored - because text-property-any
works with eq, not with equal"
  (if (string= collab-pos0-id id)
      0
    (text-property-any (point-min) (point-max) 'collab-id
		       (collab-char-id (gethash id collab-char-hash)))))

(defun collab-id-buffer-get-id (pos)
  (if (zerop pos)
      collab-pos0-id
    (get-text-property pos 'collab-id)))

(defun collab-id-buffer-insert (pos id)
  (let ((buffer-undo-list t)
	(inhibit-read-only t))
    (add-text-properties pos (1+ pos) (list 'collab-id id))))

(defun collab-id-buffer-delete-n (deleted-string)
  (loop for i from 0 below (length deleted-string)
	collect (get-text-property i 'collab-id deleted-string)))

;; char abstraction
(defun collab-char-get-id (collab-char)
  (car collab-char))

(defun collab-char-get-parent (collab-char)
  (cadr collab-char))

(defun collab-char-get-c (collab-char)
  (nth 2 collab-char))

(defun collab-char-create (id parent c)
  (list id parent c))

;; external interface (for characters arriving from another peer)
(defun collab-get-ack (client)
  (car (gethash client collab-client-hash)))

(defun collab-get-pending (client)
  (cadr (gethash client collab-client-hash)))

(defun collab-get-was-pending (client)
  (nth 2 (gethash client collab-client-hash)))

(defun collab-char-in-was-pending-p (client id)
  (let ((was-pending (collab-get-was-pending client)))
    (gethash id was-pending)))

(defun collab-char-in-pending-p (client id)
  (let ((pending (collab-get-pending client)))
    (gethash id pending)))

(defun collab-add-char-to-pending (client id)
  (let ((pending (collab-get-pending client)))
    (assert (not (gethash id pending)))
    (puthash id 1 pending)))

(defun collab-add-char-to-ack (client id)
  (let ((ack (collab-get-ack client)))
;;    (assert (not (gethash id ack)))
    (puthash id 1 ack)))

(defun collab-add-char-to-was-pending (client id)
  (let ((was-pending (collab-get-was-pending client)))
    (assert (not (gethash id was-pending)))
    (puthash id 1 was-pending)))

(defun collab-remove-char-from-pending (client id)
  (let ((pending (collab-get-pending client)))
    (assert (gethash id pending))
    (remhash id pending)))

(defun collab-send (client)
  "sends everything from ack and pending to client in sorted form. Erases ack."
;;  (loop for id being the hash-keys of pending using (hash-values flag)
;;	collect
;;	(when (= flag 2) (remhash id pending)) ;; oder alternativ (besser für Debugging): count up
;;            but now the break condition is missing...
;;            e.g. init: 2
;;            if uneven: don't send
;;            if even: send
;;	(collab-char-create ...)))
  (let ((package (loop with pending-ids = (loop for k being the hash-key of
						(collab-get-pending client) collect k)
		       with ack-ids = (loop for k being the hash-key of
					    (collab-get-ack client) collect k)
		       with all-ids = (sort (append pending-ids ack-ids) 'string<)
		       for id in all-ids
		       for sc = (gethash id collab-char-hash)
		       collect
		       (collab-char-create id
					   (collab-char-parent sc)
					   (if (= collab-del (collab-char-type sc))
					       "dd"
					     (concat "i" (collab-char-c sc)))))))
    ;; send package to client
    (when (> (length package) 0) ;; don't send empty packages
      (send-string client (prin1-to-string package))
      (clrhash (collab-get-ack client)))))

(defun collab-send-all ()
  (if collab-send-next
      (if (time-less-p (current-time) collab-send-next)
	  ;; we evaluate (current-buffer) now, this ensures
	  ;; that collab-send-all is called with the same
	  ;; current-buffer as now
	  ;; and we safe the value of collab-send-next by also
	  ;; giving it as argument to run-at-time.
	  ;; setting collab-send-next to nil indicates that a timer is
	  ;; already scheduled - the only possibility now to
	  ;; call collab-send-all is when this timer sets collab-send
	  ;; next to the old value (i.e. only one timer at a
	  ;; time is active (per buffer))
	  (progn
	    (run-at-time (time-to-seconds collab-send-delta) nil
			 (lambda (b o) (with-current-buffer b
					 (setq collab-send-next o)
					 (collab-send-all)))
			 (current-buffer)
			 collab-send-next)
	    (setq collab-send-next nil))
	(loop for client hash-key of collab-client-hash do
	      (collab-send client))
	(setq collab-send-next (time-add (current-time) collab-send-delta)))))

(defun collab-become-originator (id &optional excluded-client)
  (loop for client hash-key of collab-client-hash do
	(unless (and excluded-client (eq excluded-client client))
	  (assert (not (collab-char-in-pending-p client id)))
	  (collab-add-char-to-pending client id))))

(defun collab-deal-with-package (conn package)
  (with-current-buffer (process-get conn 'collab-buffer)
    (loop for c in package do
	  (collab-receive-char
	   conn
	   (collab-char-get-id c)
	   (collab-char-get-parent c)
	   (collab-char-get-c c)))))

(defun collab-receive-char (client id parent c)
  (if (collab-char-in-pending-p client id)
      (progn
	(collab-add-char-to-was-pending client id)
	(collab-remove-char-from-pending client id)) ;; interpret as acknowledge
    (unless (collab-char-in-was-pending-p client id) ;; only ack to client
      ;; if current client wasn't originator
      (collab-add-char-to-ack client id))
    (unless (gethash id collab-char-hash) ;; unless already processed
      ;; process it
      (collab-process-char id parent c)
      ;; and relay it to all other clients (except originator client, which gets an ack reply)
      (collab-become-originator id client))))

(defun collab-process-char (id parent c)
  (assert (and (stringp c) (= 2 (length c)) (or (= ?i (aref c 0)) (string= "dd" c)))
	  nil (format "mein fehler: id:%s parent:%s c:%s" id parent c))
  ;; add: also check, id and parent for sanity
  (if (string= "dd" c) ;; deletion
      (collab-process-delete id parent)
    (collab-process-insert id parent c))) ;; insertion

(defun collab-process-delete (id parent)
  (if (= collab-deleted (collab-char-type
			 (gethash parent collab-char-hash))) ;; if parent already deleted
      (puthash id (make-collab-char :id id :parent parent :c "dd" :type collab-del)
	       collab-char-hash) ;; then just insert it in char-hash
    (let* ((start (collab-id-buffer-get-pos parent))
	   (end (1+ start))
	   (collab-inhibit-sending t)
	   (inhibit-read-only t)
	   (collab-next-id id))
      (delete-region start end)))) ;; inserts delete character in char-hash with right id
                                   ;; via before change / after change functions

(defun collab-process-insert (id parent c)
  ;; calculate here where to insert
  ;; also has to deal with the case that parent
  ;; is deleted
  (save-excursion
    (goto-char (1+ (collab-id-buffer-get-pos parent)))
    (let ((collab-inhibit-sending t)
	  (inhibit-read-only t)
	  (collab-next-id id))
      (insert (substring c 1)))))

;; network code
(defun collab-server-start ()
  "starts a server process (if the process does not already exist)"
  (interactive)
  (if (process-status "collab-server")
      (message "collab-server already running, listening on port %i " collab-server-port)
    (make-network-process :name "collab-server"
			  :service collab-server-port
			  :family 'ipv4
			  :sentinel 'collab-server-sentinel
			  :filter 'collab-server-filter
			  :server t
			  :coding 'utf-8)
    (message "collab-server started, listening on port %i " collab-server-port)
    (setq collab-clients ())))

(defun collab-make-connection-buffer (conn)
  "Makes a read only buffer with an initial
string in it and sets it as process buffer for connection conn."
  (set-process-buffer conn (generate-new-buffer " collab-connection-buffer"))
  (process-put conn 'collab-start-read-marker (make-marker))

  (with-current-buffer (process-buffer conn)
    (insert collab-id-buffer-preamble)
    (set-marker (process-mark conn) (point))
    (set-marker (process-get conn 'collab-start-read-marker) (point))
    (setq buffer-read-only t)))

(defun collab-connect (ip-address buffer-name)
  (interactive "sIP address (default: 127.0.0.1): 
sBuffer name on %s (default: *scratch*): ")
  (unless collab-char-hash
    (collab))
  (when (string= "" ip-address)
    (setq ip-address "127.0.0.1"))
  (when (string= "" buffer-name)
    (setq buffer-name "*scratch*"))
  (let ((conn (make-network-process :name "collab-connection"
				    :host ip-address
				    :service collab-server-port
				    :family 'ipv4
				    :filter 'collab-client-filter
				    :coding 'utf-8)))
    ;; do this here, because when client filter function is called the
    ;; value of (current-buffer) could have changed
    (process-put conn 'collab-buffer (current-buffer))
    (collab-make-connection-buffer conn)
    (send-string conn (prin1-to-string (format "REQ: %sREQ_END" buffer-name)))))

(defun collab-terminate-all-network-connections ()
  (interactive)
  (mapc 'delete-process (process-list)))

(defun collab-server-stop ()
  "stops the collab server process (if existing)"
  (interactive)
  (while collab-clients
    (delete-process (car collab-clients))
    (pop collab-clients))
  (delete-process "collab-server"))

(defmacro delq-1 (e l)
  "Does (delq e l) and sets l to the result. l can be any setf-able place"
  (eval-when-compile 'cl)
  `(callf2 delq ,e ,l))

(defmacro delq-p (e l)
  "(delq-1 e l) and returns t if l changed, nil otherwise"
  `(> (length ,l) (length (delq-1 ,e ,l))))
  
(defun collab-server-sentinel (proc msg)
  (if (string= "collab-server" (process-name proc))
      (message "collab-server stopped.")
    (if (delq-p proc collab-clients)
	(message "client %s disconnected" proc)
      (push proc collab-clients)
      (collab-make-connection-buffer proc))))

(defun collab-deal-with-buffer-request-2 (requested-buffer)
  "return value: 1: success (buffer exists and is running collab)
-1: failure: buffer does not exist
-2: failure: buffer exists but is not running collab"
  (cond ((not requested-buffer) -1)
	((not (buffer-local-value 'collab-char-hash requested-buffer)) -2)
	(t 1)))

(defun collab-deal-with-buffer-request-1 (conn requested-buffer-name)
  (let* ((replies '(( 1 . "ACK: Buffer %s exists at peer and is running collab.ACK_END")
		    (-1 . "NACK: Buffer %s does not exist at peer.NACK_END")
		    (-2 . "NACK: Buffer %s exists at peer but not running collab.NACK_END")))
	 (requested-buffer (get-buffer requested-buffer-name))
	 (reply-number (collab-deal-with-buffer-request-2 requested-buffer))
	 (reply (cdr (assq reply-number replies))))
    (when (= 1 reply-number)
      ;; handshake was successful
      (process-put conn 'collab-buffer requested-buffer)
      (process-put conn 'collab-msg-buf "")
      (with-current-buffer requested-buffer
	;; can be substituted with only one hash table
	(puthash conn (list (make-hash-table :test 'equal)
			    (make-hash-table :test 'equal)
			    (make-hash-table :test 'equal))
		 collab-client-hash)))
    (send-string conn (prin1-to-string (format reply requested-buffer-name)))))

(defun collab-deal-with-buffer-request (conn request-string)
  (when (string-match "^\\(REQ: \\(.*\\)REQ_END\\)" request-string)
    (collab-deal-with-buffer-request-1 conn (match-string 2 request-string))))
  
(defun collab-read-msg (conn msg string-function cons-function)
  "writes msg to connection buffer and tries to read from it
string-function and cons-function are handlers: if the
result of the read is a string, string-function is called with
conn and result as an argument. If the result is a cons cell, cons-function
is called with conn and result as an argument."
  (collab-update-process-buffer conn msg)
  (let (read-result)
    (while (setq read-result (collab-read-from-process-buffer conn))
      (cond
       ((stringp read-result)
	(funcall string-function conn read-result))
       ((consp read-result)
	(funcall cons-function conn read-result))))))

(defun collab-server-filter (conn msg)
  (collab-read-msg conn
		   msg
		   'collab-deal-with-buffer-request
		   'collab-deal-with-package))

(defun collab-update-process-buffer (conn msg)
  (with-current-buffer (process-buffer conn)
    (goto-char (process-mark conn))
    (let ((inhibit-read-only t))
      (insert msg)
      (set-marker (process-mark conn) (point)))))

(defun collab-read-from-process-buffer (conn)
  "only return something if either list of lists or flat string,
return nil otherwise"
  (with-current-buffer (process-buffer conn)
    ;; only advance collab-start-read-marker if read successful,
    ;; else try after next insert into the process buffer to read
    ;; again from this position
    (let* ((start-read-marker (process-get conn 'collab-start-read-marker))
	   (saved-position (marker-position start-read-marker))
	   (read-result (condition-case nil
			    (read start-read-marker)
			  (end-of-file nil)
			  (invalid-read-syntax nil))))

      (if (and read-result (or (and (consp read-result) (consp (car read-result)))
			       (stringp read-result)))
	  read-result
	(set-marker start-read-marker saved-position)
	nil))))

(defun collab-client-filter (conn msg)
  (collab-read-msg conn
		   msg
		   'collab-deal-with-ack
		   'collab-deal-with-package))

(defun collab-deal-with-ack (conn ack-string)
  (when (string-match "^\\(ACK: \\(.*\\)ACK_END\\)" ack-string)
    (with-current-buffer (process-get conn 'collab-buffer)
      (puthash conn (list (make-hash-table :test 'equal)
			  (make-hash-table :test 'equal)
			  (make-hash-table :test 'equal))
	       collab-client-hash)
      (message "client: handshake ok (%s)" (match-string 2 msg))))
  (when (string-match "^\\(NACK: \\(.*\\)NACK_END\\)" msg)
    (message "client: handshake not ok (%s)" (match-string 2 msg))
    (delete-process conn)))
		  
(defun collab-list-connections ()
  "List the connections for this buffer."
  (interactive)
  (message "%s" (loop for k hash-key of collab-client-hash collect k)))

(defun collab-kill-all-replay-buffers ()
  (interactive)
  (mapc 'kill-buffer
	(remove-if-not (lambda (b)
			 (string-match "collab-replay" (buffer-name b)))
		       (buffer-list))))
