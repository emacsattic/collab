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

(defstruct simacs-char id parent undertaker c type)

(defconst simacs-in-buffer 0
  "c is an insertable character and not deleted.")
(defconst simacs-deleted 1
  "c is an insertable character and deleted.")
(defconst simacs-del 2
  "c is the special character DEL (delete)")
(defconst simacs-anchor-char 0
  "'Position' of the beginning of the text. If a character is inserted at
position 1, the anchor-char is its parent character.")
(defconst simacs-replay-delta 0.03)
(defconst simacs-replay-direction 1)
(defconst simacs-send-delta 0.33)
(defvar simacs-last-send nil)

(defvar simacs-server-process nil)
(defvar simacs-server-port 8000)
(defvar simacs-clients nil)
(defvar simacs-inhibit-sending nil)
(defvar simacs-client-hash nil)

(defvar simacs-char-hash nil
  "All characters, even deleted characters and DEL special characters are stored here.
The key is the id of the character.
buffer-local for each buffer running simacs.")
(defvar simacs-local-pc nil
  "Counter (pc = program counter) that counts all characters issued by this client.
buffer-local for each buffer running simacs.")
(defvar simacs-before-change-start nil
  "buffer local")
(defvar simacs-subsequent-before-change-calls nil
  "Sometimes before-change is called more than once in one change - this variable makes it
possible to deal with that.
buffer-local for each buffer running simacs.")
(defvar simacs-deleted-string nil
  "If the user deletes a part of the buffer, this variable stores it before the buffer change.
buffer-local for each buffer running simacs.")
(defconst simacs-id-buffer-preamble
  "This buffer is created and used by simacs - please do not modify or kill.\n"
  "String that appears at the beginning of each simacs-id-buffer.")
(defconst simacs-id-buffer-offset
  (string-bytes simacs-id-buffer-preamble)
  "Number of bytes to ignore at the beginning of a simacs-id-buffer.")

(defconst simacs-id-width 26)
(defconst simacs-char-width (+ 2 (* 2 simacs-id-width)))
(defconst simacs-pos0-id (make-string simacs-id-width ?0))

(defvar simacs-next-id nil
  "buffer-local for each buffer.")

(defun simacs-make-timestamp ()
  (apply 'format "%04x%04x%06d" (current-time)))

(defun simacs-make-id (timestamp local-pc)
  (or simacs-next-id
      (format "%s%06x%06x"
	      timestamp local-pc
	      (random #x1000000))))

(defun simacs-before-change (start end)
  (unless simacs-subsequent-before-change-calls
    (setq simacs-deleted-string (buffer-substring start end) ;; preserve properties
	  simacs-before-change-start start
	  simacs-subsequent-before-change-calls t)))

(defun* simacs-after-change (start end lll)
  (setq simacs-subsequent-before-change-calls nil)
  (let* ((inserted-string (buffer-substring-no-properties start end))
	 (timestamp (simacs-make-timestamp)))
    (when (string= simacs-deleted-string inserted-string)
      (return-from simacs-after-change))
    ;; both deletion and insertion possible in one change (when in 
    ;; overwrite mode)! Therefore two conditionals (unless/when), not one
    ;; like if/cond (they have "mutually exclusive semantics").
    (unless (zerop lll) ;; deletion
      (loop for deleted-id in (simacs-id-buffer-delete-n 
			       ;; important for M-c (capitalize-word) etc.,
			       ;; because the deleted/inserted (capitalized) string
			       ;; might be smaller than reported by before-change
			       ;; example: blubber -> Blubber
			       ;; before-change says that "blubber" is deleted
			       ;; but Emacs 23 only deletes the first b
			       ;; (earlier Emacs versions are different: they delete
			       ;; the whole word and insert the whole (modified) word
			       ;; back in)
			       (substring simacs-deleted-string
					  (- start simacs-before-change-start)
					  (+ lll (- start simacs-before-change-start))))
	    for id = (simacs-make-id timestamp (incf simacs-local-pc))
	    do
	    (setf (simacs-char-type (gethash deleted-id simacs-char-hash)) simacs-deleted)
	    (puthash
	     id
	     (make-simacs-char :id id :parent deleted-id :c "dd" :type simacs-del)
	     simacs-char-hash)
	    (unless simacs-inhibit-sending
	      (simacs-become-originator id))))
    (when (< start end) ;; insertion
      (simacs-add-insertion-properties start end timestamp)))
  (simacs-send-all)) ;; package complete, send it away!

(defun simacs-add-insertion-properties (start end timestamp)
  (loop for pos from start below end
	for parent-pos = (1- pos)
	for parent-id = (simacs-id-buffer-get-id parent-pos)
	for id = (simacs-make-id timestamp (incf simacs-local-pc))
	for c = (buffer-substring-no-properties pos (1+ pos))
	do
	  (puthash
	   id
	   (make-simacs-char :id id :parent parent-id :c c :type simacs-in-buffer)
	   simacs-char-hash)
	  (simacs-id-buffer-insert pos id)
	  (unless simacs-inhibit-sending
	    (simacs-become-originator id))))

(defun simacs-add-hooks ()
  ;; make hooks buffer-local
  (add-hook 'after-change-functions 'simacs-after-change nil t)
  (add-hook 'before-change-functions 'simacs-before-change nil t))

(defun simacs-remove-hooks ()
  ;; only remove buffer-local hooks
  (remove-hook 'before-change-functions 'simacs-before-change t)
  (remove-hook 'after-change-functions 'simacs-after-change t))

(defun simacs ()
  (interactive)
  (set (make-local-variable 'simacs-char-hash) (make-hash-table :test 'equal
								:size 10000))
  (set (make-local-variable 'simacs-local-pc) 0)
  (set (make-local-variable 'simacs-subsequent-before-change-calls) nil)
  (set (make-local-variable 'simacs-deleted-string) nil)
  (set (make-local-variable 'simacs-next-id) nil)
  (set (make-local-variable 'simacs-inhibit-sending) nil)
  (set (make-local-variable 'simacs-client-hash) (make-hash-table :test 'eq))
  (set (make-local-variable 'simacs-before-change-start) nil)
  (set (make-local-variable 'simacs-last-send) (current-time))
  ;; make the buffer-local variables immune against a major mode change:
  (put 'simacs-char-hash 'permanent-local t)
  (put 'simacs-local-pc 'permanent-local t)
  (put 'simacs-subsequent-before-change-calls 'permanent-local t)
  (put 'simacs-before-change-start 'permanent-local t)
  (put 'simacs-deleted-string 'permanent-local t)
  (put 'simacs-next-id 'permanent-local t)
  (put 'simacs-inhibit-sending 'permanent-local t)
  (put 'simacs-client-hash 'permanent-local t)
  (put 'simacs-last-send 'permanent-local t)

  (simacs-add-hooks)
  (let ((saved-buffer-chars-modified-tick (buffer-chars-modified-tick))
	(saved-buffer-modified-p (buffer-modified-p)))
    (simacs-add-insertion-properties (point-min) (point-max) (simacs-make-timestamp))
    ;; if modified flag of buffer was nil before and only
    ;; text property changes occured then don't treat this as modification
    (if (and (not saved-buffer-modified-p)                       
	     (= saved-buffer-chars-modified-tick (buffer-chars-modified-tick)))
	(set-buffer-modified-p nil)))
;;  (simacs-server-start)
;;  (run-at-time t 3 (lambda (b)
;;		     (with-current-buffer b
;;		       (simacs-send-all))) (current-buffer))
)

(defun simacs-replay ()
  (interactive)
  ;; simacs-char-hash (and simacs-char-hash-tmp, which is only a reference
  ;; to it) can grow during replay, as new input might arrive
  ;; during sit-for intervals. But the sorting of ids at the
  ;; beginning of this function has the effect of taking a snapshot,
  ;; i.e. all input arriving after snapshotting is not replayed
  (let ((history (sort (loop for k being the hash-key of simacs-char-hash collect k) 'string<))
	(simacs-char-hash-tmp simacs-char-hash))
    (with-current-buffer (generate-new-buffer "simacs-replay")
      (with-selected-window (display-buffer (current-buffer))
	(simacs)
	(loop with simacs-inhibit-sending = t
	      with inhibit-read-only = t ;; paranoia
	      for id in history
	      for char = (gethash id simacs-char-hash-tmp)
	      for parent-position = (simacs-id-buffer-get-pos (simacs-char-parent char))
	      for simacs-next-id = id
	      do
	      (sit-for simacs-replay-delta)
	      (if (= (simacs-char-type char) simacs-del) ;; deletion
		  (progn (goto-char parent-position)
			 (delete-char 1))
		(goto-char (1+ parent-position)) ;; insertion
		(insert (simacs-char-c char))))))))

(defun simacs-replay-fun (replay-buffer simacs-char-hash-tmp i h ovl)
  "the name should be changed and get a buffer prefix"
  (when (or (= i (length h)) (= i -1))
    (global-set-key "+" 'self-insert-command)
    (global-set-key "-" 'self-insert-command)
    (global-set-key "<" 'self-insert-command)
    (global-set-key ">" 'self-insert-command)
    (simacs-replay-forward)
    (message "%s" i))

  (when (and (>= i 0) (< i (length h)))
    (with-current-buffer replay-buffer
      (save-excursion
	(let* ((id (aref h i))
	       (inhibit-read-only t)
	       (simacs-inhibit-sending t)
	       (char (gethash id simacs-char-hash-tmp))
	       (parent-position (if (and (= -1 simacs-replay-direction)
					 (= (simacs-char-type char) simacs-del))
				    (progn
				      (simacs-id-buffer-get-pos
				       (simacs-char-undertaker
					(gethash (simacs-char-parent char)
						 simacs-char-hash-tmp))))
				  (simacs-id-buffer-get-pos (simacs-char-parent char))))
	       (simacs-next-id id)) 
	                            ;; if backwards:... -> don't set!
	  (when (= 1 simacs-replay-direction)
	    (if (= (simacs-char-type char) simacs-del) ;; deletion
		(progn 
		  (setf (simacs-char-undertaker (gethash (simacs-char-parent char)
							 simacs-char-hash-tmp))
			(simacs-id-buffer-get-id (1- parent-position)))
		  (goto-char parent-position)
		  (delete-char 1)
		  (move-overlay ovl parent-position (1+ parent-position)))
	      (goto-char (1+ parent-position)) ;; insertion
	      (insert (simacs-char-c char))
	      (move-overlay ovl (+ 2 parent-position) (+ 3 parent-position))))
	  (when (= -1 simacs-replay-direction)
	    (let ((inhibit-modification-hooks t))
	      (if (= (simacs-char-type char) simacs-del)
		  (progn ;; undo deletion: insert parent of del-char after undertaker:-)
		    ;; parent-position holds undertaker-position...
		    (goto-char (1+ parent-position))
		    (insert (propertize
			     (simacs-char-c (gethash (simacs-char-parent char)
						     simacs-char-hash-tmp))
			     'simacs-id (simacs-char-parent char)))
		    (move-overlay ovl (+ 2 parent-position) (+ 3 parent-position)))
		;; undo insertion
		(goto-char (1+ parent-position))
		(delete-char 1)
		(move-overlay ovl (1+ parent-position) (+ 2 parent-position)))))))))
  (run-at-time simacs-replay-delta
	       nil 'simacs-replay-fun replay-buffer simacs-char-hash-tmp
	       (+ simacs-replay-direction i) h ovl))

(defun simacs-increase-replay-speed ()
  "sollte buffer-local sein!"
  (interactive)
  (setq simacs-replay-delta (/ simacs-replay-delta 2)))

(defun simacs-decrease-replay-speed ()
  "sollte buffer-local sein!"
  (interactive)
  (setq simacs-replay-delta (* simacs-replay-delta 2)))

(defun simacs-replay-backward ()
  (interactive)
  (setq simacs-replay-direction -1)
  (message "simacs replay-backward"))

(defun simacs-replay-forward ()
  (interactive)
  (setq simacs-replay-direction 1)
  (message "simacs replay-forward"))

(defun simacs-replay-async ()
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
  ;; - simacs-char-hash (and simacs-char-hash-tmp, which is only a reference
  ;;   to it) can grow during replay, as new input might arrive
  ;;   during sit-for intervals. But the sorting of ids at the beginning
  ;;   of this function has the effect of taking a snapshot,
  ;;   i.e. all input arriving after snapshotting is not replayed
  (let* ((history (apply 'vector
			 (sort
			  (loop for k being the hash-key of simacs-char-hash collect k) 'string<)))
	 (simacs-char-hash-tmp simacs-char-hash)
	 (replay-buffer (generate-new-buffer "simacs-replay"))
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
      (simacs))
    (global-set-key "+" 'simacs-increase-replay-speed)
    (global-set-key "-" 'simacs-decrease-replay-speed)
    (global-set-key ">" 'simacs-replay-forward)
    (global-set-key "<" 'simacs-replay-backward)
    (simacs-replay-fun replay-buffer simacs-char-hash-tmp 0 history ovl)))

;; id-buffer
(defun simacs-id-buffer-get-pos (id)
  "first looks up the char in simacs-char-hash to get the id
that was used when the char was stored - because text-property-any
works with eq, not with equal"
  (if (string= simacs-pos0-id id)
      0
    (text-property-any (point-min) (point-max) 'simacs-id
		       (simacs-char-id (gethash id simacs-char-hash)))))

(defun simacs-id-buffer-get-id (pos)
  (if (zerop pos)
      simacs-pos0-id
    (get-text-property pos 'simacs-id)))

(defun simacs-id-buffer-insert (pos id)
  (let ((buffer-undo-list t)
	(inhibit-read-only t))
    (add-text-properties pos (1+ pos) (list 'simacs-id id))))

(defun simacs-id-buffer-delete-n (deleted-string)
  (loop for i from 0 below (length deleted-string)
	collect (get-text-property i 'simacs-id deleted-string)))

;; char abstraction
(defun simacs-char-get-id (simacs-char)
  (car simacs-char))

(defun simacs-char-get-parent (simacs-char)
  (cadr simacs-char))

(defun simacs-char-get-c (simacs-char)
  (nth 2 simacs-char))

(defun simacs-char-create (id parent c)
  (list id parent c))

;; external interface (for characters arriving from another peer)
(defun simacs-get-ack (client)
  (car (gethash client simacs-client-hash)))

(defun simacs-get-pending (client)
  (cadr (gethash client simacs-client-hash)))

(defun simacs-get-was-pending (client)
  (nth 2 (gethash client simacs-client-hash)))

(defun simacs-char-in-was-pending-p (client id)
  (let ((was-pending (simacs-get-was-pending client)))
    (gethash id was-pending)))

(defun simacs-char-in-pending-p (client id)
  (let ((pending (simacs-get-pending client)))
    (gethash id pending)))

(defun simacs-add-char-to-pending (client id)
  (let ((pending (simacs-get-pending client)))
    (assert (not (gethash id pending)))
    (puthash id 1 pending)))

(defun simacs-add-char-to-ack (client id)
  (let ((ack (simacs-get-ack client)))
;;    (assert (not (gethash id ack)))
    (puthash id 1 ack)))

(defun simacs-add-char-to-was-pending (client id)
  (let ((was-pending (simacs-get-was-pending client)))
    (assert (not (gethash id was-pending)))
    (puthash id 1 was-pending)))

(defun simacs-remove-char-from-pending (client id)
  (let ((pending (simacs-get-pending client)))
    (assert (gethash id pending))
    (remhash id pending)))

(defun simacs-send (client)
  "sends everything from ack and pending to client in sorted form. Erases ack."
;;  (loop for id being the hash-keys of pending using (hash-values flag)
;;	collect
;;	(when (= flag 2) (remhash id pending)) ;; oder alternativ (besser für Debugging): count up
;;            but now the break condition is missing...
;;            e.g. init: 2
;;            if uneven: don't send
;;            if even: send
;;	(simacs-char-create ...)))
  (let ((package (loop with pending-ids = (loop for k being the hash-key of
						(simacs-get-pending client) collect k)
		       with ack-ids = (loop for k being the hash-key of
					    (simacs-get-ack client) collect k)
		       with all-ids = (sort (append pending-ids ack-ids) 'string<)
		       for id in all-ids
		       for sc = (gethash id simacs-char-hash)
		       collect
		       (simacs-char-create id
					   (simacs-char-parent sc)
					   (if (= simacs-del (simacs-char-type sc))
					       "dd"
					     (concat "i" (simacs-char-c sc)))))))
    ;; send package to client
    (when (> (length package) 0) ;; don't send empty packages
      (send-string client (prin1-to-string package))
      (clrhash (simacs-get-ack client)))))

(defun simacs-send-all ()
  (when (> (float-time
	    (time-subtract (current-time) simacs-last-send))
	   simacs-send-delta)
    (loop for client hash-key of simacs-client-hash do
	  (simacs-send client))
    (setq simacs-last-send (current-time))))

(defun simacs-become-originator (id &optional excluded-client)
  (loop for client hash-key of simacs-client-hash do
	(unless (and excluded-client (eq excluded-client client))
	  (assert (not (simacs-char-in-pending-p client id)))
	  (simacs-add-char-to-pending client id))))

(defun simacs-deal-with-package (conn package)
  (with-current-buffer (process-get conn 'simacs-buffer)
    (loop for c in package do
	  (simacs-receive-char
	   conn
	   (simacs-char-get-id c)
	   (simacs-char-get-parent c)
	   (simacs-char-get-c c)))))

(defun simacs-receive-char (client id parent c)
  (if (simacs-char-in-pending-p client id)
      (progn
	(simacs-add-char-to-was-pending client id)
	(simacs-remove-char-from-pending client id)) ;; interpret as acknowledge
    (unless (simacs-char-in-was-pending-p client id) ;; only ack to client
      ;; if current client wasn't originator
      (simacs-add-char-to-ack client id))
    (unless (gethash id simacs-char-hash) ;; unless already processed
      ;; process it
      (simacs-process-char id parent c)
      ;; and relay it to all other clients (except originator client, which gets an ack reply)
      (simacs-become-originator id client))))

(defun simacs-process-char (id parent c)
  (assert (and (stringp c) (= 2 (length c)) (or (= ?i (aref c 0)) (string= "dd" c)))
	  nil (format "mein fehler: id:%s parent:%s c:%s" id parent c))
  ;; add: also check, id and parent for sanity
  (if (string= "dd" c) ;; deletion
      (simacs-process-delete id parent)
    (simacs-process-insert id parent c))) ;; insertion

(defun simacs-process-delete (id parent)
  (if (= simacs-deleted (simacs-char-type
			 (gethash parent simacs-char-hash))) ;; if parent already deleted
      (puthash id (make-simacs-char :id id :parent parent :c "dd" :type simacs-del)
	       simacs-char-hash) ;; then just insert it in char-hash
    (let* ((start (simacs-id-buffer-get-pos parent))
	   (end (1+ start))
	   (simacs-inhibit-sending t)
	   (inhibit-read-only t)
	   (simacs-next-id id))
      (delete-region start end)))) ;; inserts delete character in char-hash with right id
                                   ;; via before change / after change functions

(defun simacs-process-insert (id parent c)
  ;; calculate here where to insert
  ;; also has to deal with the case that parent
  ;; is deleted
  (save-excursion
    (goto-char (1+ (simacs-id-buffer-get-pos parent)))
    (let ((simacs-inhibit-sending t)
	  (inhibit-read-only t)
	  (simacs-next-id id))
      (insert (substring c 1)))))

;; network code
(defun simacs-server-start ()
  "starts a server process (if the process does not already exist)"
  (interactive)
  (if (process-status "simacs-server")
      (message "simacs-server already running, listening on port %i " simacs-server-port)
    (make-network-process :name "simacs-server"
			  :service simacs-server-port
			  :family 'ipv4
			  :sentinel 'simacs-server-sentinel
			  :filter 'simacs-server-filter
			  :server t
			  :coding 'utf-8)
    (message "simacs-server started, listening on port %i " simacs-server-port)
    (setq simacs-clients ())))

(defun simacs-make-connection-buffer (conn)
  "Makes a read only buffer with an initial
string in it and sets it as process buffer for connection conn."
  (set-process-buffer conn (generate-new-buffer " simacs-connection-buffer"))
  (process-put conn 'simacs-start-read-marker (make-marker))

  (with-current-buffer (process-buffer conn)
    (insert simacs-id-buffer-preamble)
    (set-marker (process-mark conn) (point))
    (set-marker (process-get conn 'simacs-start-read-marker) (point))
    (setq buffer-read-only t)))

(defun simacs-connect (ip-address buffer-name)
  (interactive "sIP address (default: 127.0.0.1): 
sBuffer name on %s (default: *scratch*): ")
  (unless simacs-char-hash
    (simacs))
  (when (string= "" ip-address)
    (setq ip-address "127.0.0.1"))
  (when (string= "" buffer-name)
    (setq buffer-name "*scratch*"))
  (let ((conn (make-network-process :name "simacs-connection"
				    :host ip-address
				    :service simacs-server-port
				    :family 'ipv4
				    :filter 'simacs-client-filter
				    :coding 'utf-8)))
    ;; do this here, because when client filter function is called the
    ;; value of (current-buffer) could have changed
    (process-put conn 'simacs-buffer (current-buffer))
    (simacs-make-connection-buffer conn)
    (send-string conn (prin1-to-string (format "REQ: %sREQ_END" buffer-name)))))

(defun simacs-terminate-all-network-connections ()
  (interactive)
  (mapc 'delete-process (process-list)))

(defun simacs-server-stop ()
  "stops the simacs server process (if existing)"
  (interactive)
  (while simacs-clients
    (delete-process (car simacs-clients))
    (pop simacs-clients))
  (delete-process "simacs-server"))

(defmacro delq-1 (e l)
  "Does (delq e l) and sets l to the result. l can be any setf-able place"
  (eval-when-compile 'cl)
  `(callf2 delq ,e ,l))

(defmacro delq-p (e l)
  "(delq-1 e l) and returns t if l changed, nil otherwise"
  `(> (length ,l) (length (delq-1 ,e ,l))))
  
(defun simacs-server-sentinel (proc msg)
  (if (string= "simacs-server" (process-name proc))
      (message "simacs-server stopped.")
    (if (delq-p proc simacs-clients)
	(message "client %s disconnected" proc)
      (push proc simacs-clients)
      (simacs-make-connection-buffer proc))))

(defun simacs-deal-with-buffer-request-2 (requested-buffer)
  "return value: 1: success (buffer exists and is running simacs)
-1: failure: buffer does not exist
-2: failure: buffer exists but is not running simacs"
  (cond ((not requested-buffer) -1)
	((not (buffer-local-value 'simacs-char-hash requested-buffer)) -2)
	(t 1)))

(defun simacs-deal-with-buffer-request-1 (conn requested-buffer-name)
  (let* ((replies '(( 1 . "ACK: Buffer %s exists at peer and is running simacs.ACK_END")
		    (-1 . "NACK: Buffer %s does not exist at peer.NACK_END")
		    (-2 . "NACK: Buffer %s exists at peer but not running simacs.NACK_END")))
	 (requested-buffer (get-buffer requested-buffer-name))
	 (reply-number (simacs-deal-with-buffer-request-2 requested-buffer))
	 (reply (cdr (assq reply-number replies))))
    (when (= 1 reply-number)
      ;; handshake was successful
      (process-put conn 'simacs-buffer requested-buffer)
      (process-put conn 'simacs-msg-buf "")
      (with-current-buffer requested-buffer
	;; can be substituted with only one hash table
	(puthash conn (list (make-hash-table :test 'equal)
			    (make-hash-table :test 'equal)
			    (make-hash-table :test 'equal))
		 simacs-client-hash)))
    (send-string conn (prin1-to-string (format reply requested-buffer-name)))))

(defun simacs-deal-with-buffer-request (conn request-string)
  (when (string-match "^\\(REQ: \\(.*\\)REQ_END\\)" request-string)
    (simacs-deal-with-buffer-request-1 conn (match-string 2 request-string))))
  
(defun simacs-read-msg (conn msg string-function cons-function)
  "writes msg to connection buffer and tries to read from it
string-function and cons-function are handlers: if the
result of the read is a string, string-function is called with
conn and result as an argument. If the result is a cons cell, cons-function
is called with conn and result as an argument."
  (simacs-update-process-buffer conn msg)
  (let (read-result)
    (while (setq read-result (simacs-read-from-process-buffer conn))
      (cond
       ((stringp read-result)
	(funcall string-function conn read-result))
       ((consp read-result)
	(funcall cons-function conn read-result))))))

(defun simacs-server-filter (conn msg)
  (simacs-read-msg conn
		   msg
		   'simacs-deal-with-buffer-request
		   'simacs-deal-with-package))

(defun simacs-update-process-buffer (conn msg)
  (with-current-buffer (process-buffer conn)
    (goto-char (process-mark conn))
    (let ((inhibit-read-only t))
      (insert msg)
      (set-marker (process-mark conn) (point)))))

(defun simacs-read-from-process-buffer (conn)
  "only return something if either list of lists or flat string,
return nil otherwise"
  (with-current-buffer (process-buffer conn)
    ;; only advance simacs-start-read-marker if read successful,
    ;; else try after next insert into the process buffer to read
    ;; again from this position
    (let* ((start-read-marker (process-get conn 'simacs-start-read-marker))
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

(defun simacs-client-filter (conn msg)
  (simacs-read-msg conn
		   msg
		   'simacs-deal-with-ack
		   'simacs-deal-with-package))

(defun simacs-deal-with-ack (conn ack-string)
  (when (string-match "^\\(ACK: \\(.*\\)ACK_END\\)" ack-string)
    (with-current-buffer (process-get conn 'simacs-buffer)
      (puthash conn (list (make-hash-table :test 'equal)
			  (make-hash-table :test 'equal)
			  (make-hash-table :test 'equal))
	       simacs-client-hash)
      (message "client: handshake ok (%s)" (match-string 2 msg))))
  (when (string-match "^\\(NACK: \\(.*\\)NACK_END\\)" msg)
    (message "client: handshake not ok (%s)" (match-string 2 msg))
    (delete-process conn)))
		  
(defun simacs-list-connections ()
  "List the connections for this buffer."
  (interactive)
  (message "%s" (loop for k hash-key of simacs-client-hash collect k)))

(defun simacs-kill-all-replay-buffers ()
  (interactive)
  (mapc 'kill-buffer
	(remove-if-not (lambda (b)
			 (string-match "simacs-replay" (buffer-name b)))
		       (buffer-list))))
