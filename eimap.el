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

(provide 'eimap)
