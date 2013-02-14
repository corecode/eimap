(eval-and-compile
  (defun eimap-method-table-name (table-name)
    (intern (format "eimap-method-table-%s" table-name))))

(defmacro eimap-declare-dispatch-table (table-name)
  "Declare a function dispatch table."
  (let ((table (eimap-method-table-name table-name)))
    `(progn
      (defvar ,table)
      (setq ,table (make-hash-table :size 20)))))

(defmacro eimap-define-method (table method args &optional docstring &rest body)
  "Define a handler for dispatching upcalls.

ARGS should be (upcall-data data)."
  (declare (indent 3)
           (doc-string 4)
           (debug (name defun )))
  `(puthash ',method (lambda ,args ,docstring . ,body) ,(eimap-method-table-name table)))

(defmacro eimap-create-dispatch (table)
  "Returns a lambda suitable for passing to `eimap-open''s `upcall'."
  (let ((table (eimap-method-table-name table)))
    `(lambda (upcall-data method data)
       (let ((handler (gethash method ,table)))
         (if handler
             (funcall handler upcall-data data)
           (let ((handler (gethash 'default ,table)))
             (when handler
               (funcall handler upcall-data method data))))))))

(provide 'eimap-dispatch)
