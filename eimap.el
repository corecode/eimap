;;; eimap.el --- imap processing using a streaming data model

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

;;; Commentary:

;; eimap is an experiment to approach IMAP processing from a new
;; angle.

;; The protocol parser and generator are both formally generated from
;; grammars.

;; Instead of using a request-reply approach (which does not work well
;; with IMAP), eimap uses a streaming data model:

;; Data from the server is automatically parsed into usable lisp data
;; structures and directly handed to a handler for the incoming data
;; (not shown in the sample code below).

;; If the application requires other data that has not been streamed in
;; yet, it can request the server to send this data, using the
;; `eimap-request' method (see example below).

;;; Code:

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
  current-mailbox
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

(defun eimap-select (mailbox)
  (eimap-request* (eimap-data-connection eimap-data)
                  `(:method SELECT :mailbox ,mailbox)
                  :cbdata (list eimap-data mailbox)
                  :barrier 'eimap-select-start
                  :done 'eimap-select-done))

(defun eimap-select-start (cbdata)
  (destructuring-bind (eimap-data mailbox) cbdata
    (setf (eimap-data-current-mailbox eimap-data) mailbox))
  cbdata)

(defun eimap-select-done (respdata cbdata)
  (destructuring-bind (eimap-data mailbox) cbdata
    (with-current-buffer (eimap-data-message-list eimap-data)
      (let ((inhibit-read-only t))
        (erase-buffer))
      (eimap-request* (eimap-data-connection eimap-data)
                      `(:method FETCH
                                :ids (:from 1 :to *)
                                :attr (ENVELOPE FLAGS UID BODYSTRUCTURE))))))

(defun eimap-visit-item ()
  (interactive)
  (let ((data (get-text-property (point) :eimap)))
    (eimap-select (plist-get data :mailbox))))

(eimap-declare-dispatch-table eimap)

(eimap-define-method eimap connection-state (eimap-data data)
  (when (eq 'authenticated (plist-get data :state))
    (eimap-select "INBOX")
    (eimap-request '(:method LSUB :mailbox "" :pattern "*"))))

(eimap-define-method eimap LSUB (eimap-data data)
  (let ((mailbox (plist-get data :mailbox)))
    (unless (equal mailbox (eimap-data-current-mailbox eimap-data))
      (eimap-request `(:method STATUS
                               :mailbox ,mailbox
                               :attr (MESSAGES RECENT UNSEEN))))))

(eimap-define-method eimap STATUS (eimap-data data)
  (with-current-buffer (eimap-data-folder-list eimap-data)
    (let ((str (propertize
                (format "%s (%d/%d)\n"
                        (plist-get data :mailbox)
                        (plist-get data :unseen)
                        (plist-get data :messages))
                :eimap data)))
      (when (> (plist-get data :recent) 0)
        (setq str (propertize str 'face 'bold)))
      (let ((inhibit-read-only t))
        (insert str)))))

(eimap-define-method eimap FETCH (eimap-data data)
  (with-current-buffer (eimap-data-message-list eimap-data)
    (let (face)
      (when (member "\\seen" (plist-get data :flags))
        (add-to-list 'face 'shadow))
      (when (member "\\deleted" (plist-get data :flags))
        (add-to-list 'face '(:strike-through t)))
      (let ((str (propertize
                  (concat
                   (truncate-string-to-width
                    (or (multi-plist-get data :envelope :subject)
                        "(none)")
                    50 0 ?  "\u2026")
                   " "
                   (truncate-string-to-width
                    (eimap-address-string data :from)
                    15 0 ?  "\u2026")
                   "\n")
                  :eimap data
                  'face face)))
        (let ((inhibit-read-only t))
          (insert str))))))

(eimap-define-method eimap default (eimap-data method data)
  (message "IMAP %s %s"
           method (pp-to-string data)))

(defun eimap-address-string (data field)
  (let* ((fdata (multi-plist-get data :envelope field))
         (first (car fdata)))
    (or (plist-get first :name)
        (concat (or (plist-get first :mailbox) "(none)")
                "@"
                (or (plist-get first :host) "(none)")))))

(defun multi-plist-get (plist &rest keys)
  (mapcar (lambda (k)
            (setq plist (plist-get plist k)))
          keys)
  plist)

(provide 'eimap)
;;; eimap.el ends here
