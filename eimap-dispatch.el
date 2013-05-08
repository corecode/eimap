;;; eimap-dispatch.el ---

;; Copyright (C) 2013  Simon Schubert

;; Author: Simon Schubert <2@0x2c.org
;; Created: 20130116

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Code:

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
;;; eimap-dispatch.el ends here
