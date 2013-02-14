(require 'cl)
(require 'eimap-connection)
(require 'eimap-dispatch)

(defvar eimap-mode-map
  (let ((map (make-keymap)))
    (suppress-keymap map t)
    (define-key map (kbd "n") 'next-line)
    (define-key map (kbd "p") 'previous-line)
    (define-key map (kbd "RET") 'eimap-visit-item)
    map))

(defun eimap-mode ()
  (kill-all-local-variables)
  (buffer-disable-undo)
  (setq buffer-read-only t
        truncate-lines t
        major-mode 'eimap-mode
        mode-name "eimap"
        mode-line-process "")
  (use-local-map eimap-mode-map)
  (add-hook (make-local-variable 'kill-buffer-hook) 'eimap-kill))

(defun eimap (host &rest rest)
  (switch-to-buffer (get-buffer-create "*eimap*"))
  (eimap-mode)
  (set (make-local-variable 'eimap-conn)
       (apply #'eimap-open
              host
              :upcall (eimap-create-dispatch eimap)
              :upcall-data (current-buffer)
              rest)))

(defun eimap-kill ()
  (condition-case e
      (eimap-close eimap-conn)
    (error nil)))

(defun eimap-visit-item ()
  (interactive)
  (let ((data (get-text-property (point) :eimap)))
    (eimap-request* eimap-conn `(:method SELECT :mailbox ,(plist-get data :mailbox)))))

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
    (let ((str (propertize
                (format "%s (%d/%d)\n"
                        (plist-get data :mailbox)
                        (plist-get data :unseen)
                        (plist-get data :messages))
                :eimap data)))
      (when (> (plist-get data :recent) 0)
        (setq str (propertize str 'face '(:weight bold))))
      (let ((inhibit-read-only t))
        (insert str)))))

(eimap-define-method eimap default (buf method data)
  (message "IMAP %s %s"
           method (pp-to-string data)))

(provide 'eimap)
