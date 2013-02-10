(require 'cl)
(require 'sasl)
(require 'eimap-parse)
(require 'eimap-generate)

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
    (set (make-local-variable 'eimap-continue-tag) nil)
    (set (make-local-variable 'eimap-outstanding-tags) nil)
    (set (make-local-variable 'eimap-req-queue) nil)
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
  "Generate new IMAP tag"
  (setq eimap-tag (1+ eimap-tag))
  (concat "e" (number-to-string eimap-tag)))

(defun* eimap-request (req &rest data &key continue done cbdata barrier)
  "Tag request and setup callback hooks"
  (let* ((tag (eimap-new-tag))
         (req (plist-put req :tag tag))
         ;; eimap-generate returns a list of strings, each for one
         ;; continuation step
         (req-strs (eimap-generate req))
         (req-str (car req-strs))
         (cont-strs (cdr req-strs))
         tag-data)
    ;; prepare data for completion/continuation
    ;; if we have continuation data, set a callback for it
    (when (and cont-strs (not continue))
      (setq continue 'internal)
      (setq data (plist-put data :continue continue)))
    (setq data (plist-put data :req-str req-str))
    (setq data (plist-put data :cont-strs cont-strs))
    (setq tag-data (cons tag data))
    ;; add to request queue and kick it
    (add-to-list 'eimap-req-queue data t)
    (eimap-process-req-queue)))

(defun eimap-process-req-queue ()
  "Issue queued requests when the time is right."
  ;; do we have queued requests and all in-flight requests have drained?
  (catch 'out
    (while eimap-req-queue
      (let* ((data (pop eimap-req-queue))
             (tag (plist-get data :tag))
             (barrier (plist-get data :barrier))
             (continue (plist-get data :continue))
             (req-str (plist-get data :req-str)))
        ;; stop if:
        ;; - this is a barrier and there are already requests inflight
        ;; - we wait on a continue
        (when (or (and barrier eimap-outstanding-tags)
                  eimap-continue-tag)
          ;; we popped too soon
          (push data eimap-req-queue)
          (throw 'out nil))
        (when barrier
          (funcall barrier (plist-get data :cbdata)))
        (when continue
          (setq eimap-continue-tag tag))
        (push (cons tag data) eimap-outstanding-tags)
        (eimap-network-send req-str)))))

(defun eimap-process-reply ()
  "Parse network reply and run protocol state machine."
  (let* ((reply (eimap-parse))
         (type (plist-get reply :type))
         (params (plist-get reply :params))
         (method (plist-get reply :method)))
    (case type
      ('continue
       (let* ((tag eimap-continue-tag)
              (tag-record (assoc-string tag eimap-outstanding-tags))
              (tag-data (cdr tag-record))
              (continue (plist-get tag-data :continue))
              (cbdata (plist-get tag-data :cbdata)))
         (when (null continue)
           (error "Continuation without handler for request data %S" tag-data))
         (case continue
           ('internal
            (eimap-process-common-continue tag-data))
           (t
            (setq data (funcall continue params cbdata))
            (setcdr tag-record (plist-put tag-data :cbdata cbdata))))))
      ('data
       (let ((handler (gethash method eimap-method-dispatch-table))
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
         (setq eimap-outstanding-tags (delq tag-record eimap-outstanding-tags))
         (eimap-process-req-queue))))))

(defun eimap-process-common-continue (data)
  (let* ((cont-strs (plist-get data :cont-strs))
         (next-str (car cont-strs))
         (next-cont-strs (cdr cont-strs)))
    (assert next-str)
    ;; if there are more strings, update our data (ugly :/)
    (if next-cont-strs
        (setcdr (assoc (plist-get data :tag) eimap-outstanding-tags)
                (plist-put data :cont-strs next-cont-strs))
      ;; else no more tag: flush the queue
      (setq eimap-continue-tag nil)
      (eimap-process-req-queue))))

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
       (when (eq eimap-state 'connecting)
         (eimap-authenticate)))
      ('PREAUTH
       (when (eq state 'PREAUTH)
         (setq eimap-state 'authenticated)))
      ('BYE
       (message "server closing connection: %s" (plist-get params :text)))
      (('BAD 'NO)
       (warn "server unhappy: %s")))))

(defun eimap-authenticate ()
  "Initiate authentication"
  (if (null eimap-auth-methods)
      (eimap-request "CAPABILITY")
    (let ((mech (sasl-find-mechanism eimap-auth-methods)))
      (when (null mech)
        (error "No SASL mechanism found do handle server supported methods: %S" eimap-auth-methods))
      (set eimap-state 'authenticating)
      (let* ((client (sasl-make-client mech eimap-user eimap-port eimap-host))
             (steps nil)
             (cbdata (list :client client
                           :steps steps))
             (req (list :method 'AUTHENTICATE
                        :auth-mech (sasl-mechanism-name mech))))
        (when (member "SASL-IR" eimap-capabilities)
          (let ((reply-data (eimap-auth-next-step "" cbdata)))
            (setq req (plist-put req :auth-token (car reply-data)))
            (setq cbdata (cdr reply-data))))
        (eimap-request req
                       :continue 'eimap-auth-steps
                       :done 'eimap-auth-done
                       :cbdata cbdata)))))

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
  (case (plist-get params :state)
    (OK
     (let* ((authtok (plist-get data :authtok))
            (pw-save-fun (plist-get authtok :save-function)))
       (when pw-save-fun
         (funcall pw-save-fun)))
     (message "auth successful: %s" (plist-get params :text)))
    (t
     (error "auth fail: %s" (plist-get params :text)))))
