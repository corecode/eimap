(defun eimap/open ()
  "Open IMAP connection and set up buffer"
  (setq imapbuf (generate-new-buffer "*imap 0x2c.org*"))
  (with-current-buffer imapbuf
    (make-variable-buffer-local 'after-change-functions)
    (add-to-list 'after-change-functions 'eimap/network-recv)
    (make-variable-buffer-local 'prev-data)
    (setq prev-data (point-max))
    (make-variable-buffer-local 'recv-state)
    (setq recv-state 'reply)
    (make-variable-buffer-local 'line-with-crlf)
    (setq line-with-crlf "^\\(.*\\)\u000D\u000A")
    (make-variable-buffer-local 'eimap-connection)
    (let ((coding-system-for-read 'binary)
	  (coding-system-for-write 'binary))
      (setq eimap-connection
	    (open-network-stream
	     "imap 0x2c.org"
	     imapbuf
	     "0x2c.org" "imaps"
	     :nowait t
	     :type 'ssl)))
    (set-process-sentinel eimap-connection 'eimap/sentinel)))

(defun eimap/sentinel (conn state)
  (message "imap goes poof"))

(defun eimap/network-recv (startchg endchg prevlen)
  "Process incoming network data, either line based replies or a
defined amount of octets."
  (save-restriction
    (narrow-to-region prev-data (point-max))
    (goto-char (point-min))
    (when (eimap/network-reply-ready-p)
      (message "full reply"))))

(defun eimap/network-reply-ready-p ()
  "Wait for a complete reply from the server."
  ;; check for a complete terminated line,
  ;; optionally with a trailing literal count
  (goto-char (point-min))
  (catch 'end
    (while (looking-at "\\(.*?\\)\\(?:[{]\\([[:digit:]]+\\)[}]\\)?\u000D\u000A")
      ;; skip over literal if there is one
      (if (match-beginning 2)
	  (let* ((literal-length-str (buffer-substring (match-beginning 2)
						       (match-end 2)))
		 (literal-length (string-to-number literal-length-str))
		 (literal-end (+ (match-end 0) literal-length)))
	    ;; all literal data available?
	    (if (>= (point-max) literal-end)
		(goto-char literal-end)
	      ;; not enough data yet
	      (throw 'end nil)))
	;; else no literal
	(throw 'end t)))
    nil))
