(require 'cl)
(require 'sasl)

(defun eimap-authenticate ()
  "Initiate authentication"
  (if (null eimap-auth-methods)
      (eimap-request '(:method CAPABILITY)
                     :done 'eimap-authenticate-cb)
    (let ((mech (sasl-find-mechanism eimap-auth-methods)))
      (when (null mech)
        (error "No SASL mechanism found do handle server supported methods: %S" eimap-auth-methods))
      (eimap-set-state 'authenticating)
      (let* ((client (sasl-make-client mech eimap-user eimap-port eimap-host))
             (steps nil)
             (cbdata (list :client client
                           :steps steps))
             (req (list :method 'AUTHENTICATE
                        :auth-mech (sasl-mechanism-name mech))))
        (when (eimap-capability "SASL-IR")
          (let ((reply-data (eimap-auth-next-step "" cbdata)))
            (setq req (plist-put req :auth-token (car reply-data)))
            (setq cbdata (cdr reply-data))))
        (eimap-request req
                       :continue 'eimap-auth-steps
                       :done 'eimap-auth-done
                       :cbdata cbdata)))))

(defun eimap-authenticate-cb (data cbdata)
  (when (null eimap-auth-methods)
    (error "CAPABILITY did not return auth methods"))
  (eimap-authenticate))

(defun eimap-auth-steps (params data)
  "Continuation callback for AUTHENTICATE.  Returns NEW-DATA that gets
passed in again in the next step."
  (let* ((text (plist-get params :text))
         (reply-data (eimap-auth-next-step text data)))
    (eimap-network-send (concat (car reply-data) "\x0d\x0a"))
    (cdr reply-data)))

(defun eimap-auth-next-step (text data)
  "Take next step through SASL.  TEXT is the answer from the server.
Returns (REPLY . NEW-DATA)."
  (let* (
         (decoded (base64-decode-string text))
         (client (plist-get data :client))
         (steps (plist-get data :steps))
         ;; eimap-read-pass might set eimap-authtok
         (sasl-read-passphrase 'eimap-read-pass)
         (eimap-authtok nil)
         auth-reply)
    (setq steps (sasl-next-step client (and (> (length decoded) 0)
                                            decoded)))
    (setq data (plist-put data :steps steps))
    (setq auth-reply (base64-encode-string (sasl-step-data steps) t))
    (setq data (plist-put data :authtok eimap-authtok))
    (cons auth-reply data)))

(defun eimap-read-pass (prompt)
  "Callback from SASL to read the passphrase.  Instead of
prompting the user, we ask auth-source to return passphrase or
prompt."
  (let ((authtok (car (auth-source-search :host eimap-host
                                          :port eimap-port
                                          :user eimap-user
                                          :max 1
                                          :create t))))
    (when (null authtok)
      (error "No authentication token available"))
    (setq eimap-authtok authtok)        ; this is local in eimap-auth-steps
    (let ((secret (plist-get authtok :secret)))
      ;; make a copy here because SASL zeroes out the passphrase -
      ;; that's bad if auth-source wants to cache it.
      (copy-sequence
       (if (functionp secret)
           (funcall secret)
         secret)))))

(defun eimap-auth-done (params data)
  "Authentication completed.  Give auth-source a chance to store
the password.

XXX maybe detect if password was cached and discard on error?"
  (setq eimap-continue-tag nil)
  (eimap-set-state 'authenticated)
  (case (plist-get params :state)
    (OK
     (let* ((authtok (plist-get data :authtok))
            (pw-save-fun (plist-get authtok :save-function)))
       (when pw-save-fun
         (funcall pw-save-fun)))
     (message "auth successful: %s" (plist-get params :text)))
    (t
     (error "auth fail: %s" (plist-get params :text)))))

(provide 'eimap-auth)
