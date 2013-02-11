;;; Brief overview how to use it

(add-to-list 'load-path default-directory)

(require 'eimap)
(with-current-buffer (eimap-open "0x2c.org" :user "2") ; connects via IMAPS/993
  ;; XXX need to figure out how to do the callbacks
  (while (not (eq eimap-state :authenticated))
    (sleep-for 1))
  (eimap-request '(:method SELECT :mailbox "INBOX"))
  (eimap-request '(:method SEARCH :keys ((NOT . DELETED)))
                 :cbdata "foo"
                 :done (lambda (data cbdata)
                         (message "callback data %s cbdata %s"
                                  (pp-to-string data)
                                  (pp-to-string cbdata))))
  (eimap-request '(:method FETCH :uids (:from 1 :to *) :attr FAST))
  (switch-to-buffer (current-buffer)))
