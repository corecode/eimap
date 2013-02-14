(require 'cl)
(require 'eimap-connection)
(require 'eimap-dispatch)

(defun eimap (host &rest rest)
  (switch-to-buffer (get-buffer-create "*eimap*"))
  (add-hook (make-local-variable 'kill-buffer-hook) 'eimap-kill)
  (set (make-local-variable 'eimap-conn)
       (apply #'eimap-open
              host
              :upcall (eimap-create-dispatch eimap)
              :upcall-data (current-buffer)
              rest)))

(defun eimap-kill ()
  (eimap-close eimap-conn))

(eimap-declare-dispatch-table eimap)

(eimap-define-method eimap connection-state (buf data)
  (when (eq 'authenticated (plist-get data :state))
    (eimap-request '(:method LSUB :mailbox "" :pattern "*"))))

(eimap-define-method eimap LSUB (buf data)
  (eimap-request `(:method STATUS
                           :mailbox ,(plist-get data :mailbox)
                           :attr (MESSAGES RECENT UNSEEN))))

(eimap-define-method eimap STATUS (buf data)
  (with-current-buffer buf
    (let ((str (format "%s (%d/%d)\n"
                       (plist-get data :mailbox)
                       (plist-get data :unseen)
                       (plist-get data :messages))))
      (when (> (plist-get data :recent) 0)
        (setq str (propertize str 'face '(:weight bold))))
      (insert str))))

(eimap-define-method eimap default (buf method data)
  (message "IMAP %s %s"
           method (pp-to-string data)))

(provide 'eimap)
