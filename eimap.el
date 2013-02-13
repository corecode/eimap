(require 'cl)
(require 'eimap-parse)
(require 'eimap-generate)
(require 'eimap-connection)
(require 'eimap-auth)

(defvar eimap-method-dispatch-table)
(setq eimap-method-dispatch-table (make-hash-table :size 20))

(defmacro eimap-define-method (method args &optional docstring &rest body)
  "Define a handler for incoming :type 'data :methods."
  (declare (indent defun)
           (doc-string 3)
           (debug defun))
  `(puthash ',method (lambda ,args ,docstring . ,body) eimap-method-dispatch-table))

(defun eimap-dispatch-method (method data)
  "Execute handler for METHOD"
  (let ((handler (gethash method eimap-method-dispatch-table)))
    (if handler
        (funcall handler data)
      (message "No IMAP handler for %s:\n%s" method (pp-to-string data)))))

(eimap-define-method cond-state (data)
  "Handle untagged OK/NO/BAD/BYE/PREAUTH messages"
  (let ((state (plist-get data :state)))
    (case state
      ('OK
       t)
      ('PREAUTH
       (when (eq eimap-state :connected)
         (setq eimap-state :authenticated)))
      ('BYE
       (message "server closing connection: %s" (plist-get data :text)))
      (('BAD 'NO)
       (warn "server unhappy: %s")))))

(eimap-define-method CAPABILITY (data)
  "Handle CAPABILITY updates"
  (setq eimap-capabilities (plist-get data :capabilities)
        eimap-auth-methods (plist-get data :auth)))

(provide 'eimap)
