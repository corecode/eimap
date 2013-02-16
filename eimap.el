(require 'cl)
(require 'eimap-connection)
(require 'eimap-dispatch)

(defvar eimap-mode-map
  (let ((map (make-keymap)))
    (suppress-keymap map t)
    (define-key map (kbd "q") 'eimap-quit)
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

(put 'eimap-kill 'mode-class 'special)

(define-derived-mode eimap-folder-mode eimap-mode "eimap Folder")
(define-derived-mode eimap-message-mode eimap-mode "eimap Messages")

(defstruct eimap-data
  connection
  folder-list
  message-list)

(defun eimap (host &rest rest)
  (let ((data (make-eimap-data)))
    (setf (eimap-data-connection data)
          (apply #'eimap-open
                 host
                 :upcall (eimap-create-dispatch eimap)
                 :upcall-data data
                 rest))
    (when (null (eimap-data-connection data))
      (error "Can not establish IMAP session"))
    (window-configuration-to-register :eimap)
    (setf (eimap-data-folder-list data) (generate-new-buffer "*eimap-folders*")
          (eimap-data-message-list data) (generate-new-buffer "*eimap-messages*"))
    (with-current-buffer (eimap-data-folder-list data)
      (eimap-folder-mode)
      (set (make-local-variable 'eimap-data) data))
    (with-current-buffer (eimap-data-message-list data)
      (eimap-message-mode)
      (set (make-local-variable 'eimap-data) data))
    (switch-to-buffer (eimap-data-folder-list data))
    (delete-other-windows)
    (let ((folder-window (selected-window))
          (message-window (split-window-horizontally 20)))
      (select-window message-window)
      (switch-to-buffer (eimap-data-message-list data) nil t))))

(defun eimap-quit ()
  (interactive)
  (when (or (not (boundp 'eimap-quitting))
            (not eimap-quitting))
    (let (;; create a reference because the buffer is going away
          (data eimap-data)
          (eimap-quitting t))
      (kill-buffer (eimap-data-message-list data))
      (kill-buffer (eimap-data-folder-list data))
      (eimap-close (eimap-data-connection data))
      (jump-to-register :eimap))))

(defun eimap-kill ()
  (condition-case e
      (eimap-quit)
    (error nil)))

(defun eimap-visit-item ()
  (interactive)
  (let ((data (get-text-property (point) :eimap)))
    (eimap-request* (eimap-data-connection eimap-data)
                    `(:method SELECT :mailbox ,(plist-get data :mailbox)))))

(eimap-declare-dispatch-table eimap)

(eimap-define-method eimap connection-state (ed data)
  (when (eq 'authenticated (plist-get data :state))
    (eimap-request '(:method LSUB :mailbox "" :pattern "*"))))

(eimap-define-method eimap LSUB (ed data)
  (eimap-request `(:method STATUS
                           :mailbox ,(plist-get data :mailbox)
                           :attr (MESSAGES RECENT UNSEEN))))

(eimap-define-method eimap STATUS (ed data)
  (with-current-buffer (eimap-data-folder-list ed)
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

(eimap-define-method eimap default (ed method data)
  (message "IMAP %s %s"
           method (pp-to-string data)))

(provide 'eimap)
