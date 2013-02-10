(require 'cl)
(require 'eimap-parse)
(require 'eimap-generate)
(require 'eimap-connection)
(require 'eimap-auth)

(defvar eimap-method-dispatch-table)
(setq eimap-method-dispatch-table (make-hash-table :size 20))

(defmacro eimap-define-method (method args &optional docstring &rest body)
  (declare (indent defun)
           (doc-string 3)
           (debug defun))
  `(puthash ',method (lambda ,args ,docstring . ,body) eimap-method-dispatch-table))

(eimap-define-method cond-state (data params)
  "Handle untagged OK/NO/BAD/BYE/PREAUTH messages"
  (let ((state (plist-get data :state))
        (resp-code (plist-get data :resp-code)))
    (case resp-code
      ('CAPABILITY
       (setq eimap-capabilities (plist-get params :capabilities)
             eimap-auth-methods (plist-get params :auth))))
    (case state
      ('OK
       (when (eq eimap-state :connecting)
         (eimap-authenticate)))
      ('PREAUTH
       (when (eq state 'PREAUTH)
         (setq eimap-state :authenticated)))
      ('BYE
       (message "server closing connection: %s" (plist-get params :text)))
      (('BAD 'NO)
       (warn "server unhappy: %s")))))

(provide 'eimap)
