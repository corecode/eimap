(require 'cl)
(require 'sasl)

(defvar eimap-method-dispatch-table)
(setq eimap-method-dispatch-table (make-hash-table :size 20))

(defmacro eimap-define-method (method args &optional docstring &rest body)
  (declare (indent defun)
           (doc-string 3)
           (debug defun))
  `(puthash ',method (lambda ,args ,docstring . ,body) eimap-method-dispatch-table))


(defun eimap-sentinel (conn state)
  (message "imap goes poof"))


(defun eimap-network-recv (process output)
  "Process incoming network data, either line based replies or a
defined amount of octets."
  (with-current-buffer (process-buffer process)
    (save-excursion
      (goto-char (point-max))
      (insert output)
      (set-marker (process-mark process) (point)))
    (while (eimap-network-reply-ready-p)
      (eimap-process-reply))))

(defun eimap-network-reply-ready-p ()
  "Wait for a complete reply from the server."
  ;; check for a complete terminated line,
  ;; optionally with a trailing literal count
  (save-excursion
    (catch 'end
      (while (looking-at "\\(.+?\\)\\(?:[{]\\([[:digit:]]+\\)[}]\\)?\u000D\u000A")
        ;; skip over literal if there is one
        (if (match-beginning 2)
            (let* ((literal-length-str (buffer-substring (match-beginning 2)
                                                         (match-end 2)))
                   (literal-length (string-to-number literal-length-str))
                   (literal-end (+ (match-end 0) literal-length)))
              ;; all literal data available?
              (if (>= (point-max) literal-end)
                  (goto-char literal-end)
                ;; not enough data yet
                (throw 'end nil)))
          ;; else no literal
          (throw 'end t)))
      nil)))


(defun eimap-network-send (output)
  "Send IMAP data.  Must be called with the process buffer
active."
  (let ((outovr (make-overlay (1- (point)) (point))))
    (overlay-put outovr 'after-string
                 (propertize output 'face '(:foreground "red")))
    (process-send-string (get-buffer-process (current-buffer)) output)))


(defun* eimap-open (host &key (user (user-login-name)) (port "imaps")
                         &aux (procname (format "*imap %s*" host)))
  "Open IMAP connection and set up buffer"
  (with-current-buffer (generate-new-buffer procname)
    (set (make-local-variable 'eimap-state) 'connecting)
    (set (make-local-variable 'eimap-user) user)
    (set (make-local-variable 'eimap-host) host)
    (set (make-local-variable 'eimap-port) port)
    (set (make-local-variable 'eimap-capabilities) nil)
    (set (make-local-variable 'eimap-auth-methods) nil)
    (set (make-local-variable 'eimap-continue-tags) nil)
    (set (make-local-variable 'eimap-outstanding-tags) nil)
    (set (make-local-variable 'eimap-tag) 0)
    (let ((coding-system-for-read 'binary)
          (coding-system-for-write 'binary)
          process)
      (setq process
            (open-network-stream
             procname
             (current-buffer)
             host port
             :nowait t
             :type 'ssl))
      (set-process-filter process 'eimap-network-recv)
      (set-process-sentinel process 'eimap-sentinel)
      ;; trigger parsing of greeting
      (eimap-network-recv process ""))
    (current-buffer)))


(defun eimap-new-tag ()
  (setq eimap-tag (1+ eimap-tag))
  (concat "e" (number-to-string eimap-tag)))

(defun* eimap-request (string &rest data &key continue done cbdata)
  (let* ((tag (eimap-new-tag))
         (tag-record (cons tag data)))
    (add-to-list 'eimap-outstanding-tags tag-record)
    (when continue
      (push tag eimap-continue-tags))
    (let ((req (concat tag " " string "\x0d\x0a")))
      (eimap-network-send req))))

(defun eimap-process-reply ()
  "Parse network reply and run protocol state machine."
  (let* ((reply (eimap-parse))
         (type (plist-get reply :type))
         (params (plist-get reply :params))
         (method (plist-get reply :method)))
    (case type
      ('continue
       (let* ((tag (car eimap-continue-tags))
              (tag-record (assoc-string tag eimap-outstanding-tags))
              (tag-data (cdr tag-record))
              (cont-handler (plist-get tag-data :continue))
              (cbdata (plist-get tag-data :cbdata)))
         (when (null cont-handler)
           (error "Continuation without handler for request data %S" tag-data))
         ;; we pass on the returned data to the next handler
         (let ((cbdata (funcall cont-handler params cbdata))
               (new-tag-data (plist-put tag-data :cbdata cbdata)))
           (setcdr tag-record new-tag-data))))
      ('data
       (let* ((handler (gethash method eimap-method-dispatch-table))
              (handler-params (plist-get params :params)))
         (apply handler (list params handler-params))))
      ('tag
       (let* ((tag (plist-get params :tag))
              (tag-record (assoc-string tag eimap-outstanding-tags))
              (tag-data (cdr tag-record))
              (done-handler (plist-get tag-data :done))
              (cbdata (plist-get tag-data :cbdata)))
         (when done-handler
           (funcall done-handler params cbdata))
         (setq eimap-outstanding-tags (delq tag-record eimap-outstanding-tags)))))))

(eimap-define-method cond-state (data params)
  (let ((state (plist-get data :state))
        (resp-code (plist-get data :resp-code)))
    (case resp-code
      (CAPABILITY
       (setq eimap-capabilities (plist-get params :capabilities)
             eimap-auth-methods (plist-get params :auth))))
    (case state
      (OK
       (when (eq eimap-state 'connecting)
         (eimap-authenticate)))
      (PREAUTH
       (when (eq state 'PREAUTH)
         (setq eimap-state 'authenticated)))
      (BYE
       (message "server closing connection: %s" (plist-get params :text)))
      ((BAD NO)
       (warn "server unhappy: %s")))))

(defun eimap-authenticate ()
  (if (null eimap-auth-methods)
      (eimap-request "CAPABILITY")
    (let ((mech (sasl-find-mechanism eimap-auth-methods)))
      (when (null mech)
        (error "No SASL mechanism found do handle server supported methods: %S" eimap-auth-methods))
      (set eimap-state 'authenticating)
      (let* ((client (sasl-make-client mech eimap-user eimap-port eimap-host))
             (steps nil))
        (eimap-request (format "AUTHENTICATE %s" (sasl-mechanism-name mech))
                       :continue 'eimap-auth-steps
                       :done 'eimap-auth-done
                       :cbdata (list :client client :steps steps))))))

(defun eimap-auth-steps (params data)
  (let* ((text (plist-get params :text))
         (decoded (base64-decode-string text))
         (client (plist-get data :client))
         (steps (plist-get data :steps))
         ;; eimap-read-pass might set eimap-authtok
         (sasl-read-passphrase 'eimap-read-pass)
         (eimap-authtok nil))
    (setq steps (sasl-next-step client (and (> (length decoded) 0)
                                            decoded)))
    (setq data (plist-put data :steps steps))
    (eimap-network-send (concat (base64-encode-string (sasl-step-data steps) t) "\x0d\x0a"))
    (setq data (plist-put data :authtok eimap-authtok))
    data))

(defun eimap-auth-done (params data)
  (case (plist-get params :state)
    (OK
     (let* ((authtok (plist-get data :authtok))
            (pw-save-fun (plist-get authtok :save-function)))
       (when pw-save-fun
         (funcall pw-save-fun)))
     (message "auth successful: %s" (plist-get params :text)))
    (t
     (error "auth fail: %s" (plist-get params :text)))))

(defun eimap-read-pass (prompt)
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
