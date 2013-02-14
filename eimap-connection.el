(require 'cl)
(require 'eimap-auth)
(require 'eimap-parse)
(require 'eimap-generate)


(defun* eimap-open (host &key (user (user-login-name)) (port "imap")
                         (ssl (member port '("imaps" "993" 993)))
                         upcall upcall-data
                         &aux (procname (format "*imap %s:%s*" host port)))
  "Open IMAP connection and set up buffer"
  (with-current-buffer (generate-new-buffer procname)
    (kill-all-local-variables)
    (buffer-disable-undo)
    (setq buffer-read-only t)
    (dolist (var
             `((eimap-user ,user)
               (eimap-host ,host)
               (eimap-port ,port)
               (eimap-upcall ,upcall)
               (eimap-upcall-data ,upcall-data)
               (eimap-tag 0)
               eimap-state
               eimap-capabilities
               eimap-auth-methods
               eimap-continue-tag
               eimap-outstanding-tags
               eimap-req-queue
               eimap-recv-mark))
      (set (make-local-variable (if (listp var)
                                    (nth 0 var)
                                  var))
           (and (listp var)
                (nth 1 var))))
    (let* ((coding-system-for-read 'binary)
           (coding-system-for-write 'binary)
           open-args process)
      ;; we only set the whole shebang for starttls so that we can
      ;; observe the greeting for ssl
      (if ssl
          (setq open-args `(:type ssl))
        (setq open-args
              (list :end-of-command "^.*\r\n"
                    :capability-command "x1 CAPABILITY\r\n"
                    :end-of-capability "^x1 .*\r\n"
                    :starttls-function
                    (lambda (cap)
                      "x2 STARTTLS\r\n")
                    :success "^x2 OK.*\r\n")))
      (eimap-set-state 'connecting)
      (setq process (apply #'open-network-stream
                           procname
                           (current-buffer)
                           host port
                           :nowait t
                           open-args))
      (when (null process)
        (error "Could not connect to %s@%s:%s" user host port))
      (set-process-filter process 'eimap-network-recv)
      (set-process-sentinel process 'eimap-sentinel)
      (eimap-set-state 'connected)
      ;; trigger parsing of greeting
      (setq eimap-recv-mark (point-marker))
      (eimap-network-recv process "")
      (unless (eq eimap-state 'authenticated)
        (eimap-authenticate)))
    (current-buffer)))

(defun eimap-close (conn &optional keep-buffer)
  "Close connection CONN and remove associated buffer.  Keeps the
buffer if KEEP-BUFFER is not nil."
  (with-current-buffer conn
    (if (not (memq eimap-state '(closing closed)))
        (progn
          (eimap-set-state 'closing)
          (eimap-request '(:method LOGOUT)
                         :cbdata keep-buffer
                         :done (lambda (data keep-buffer)
                                 (eimap-close-1 keep-buffer))))
      (eimap-close-1 keep-buffer))))

(defun eimap-close-1 (keep-buffer)
  "Closes the network process and removes the buffer if
KEEP-BUFFER is nil."
  (eimap-set-state 'closed)
  (let ((proc (get-buffer-process (current-buffer))))
    (when proc
      (delete-process proc)))
  (when (not keep-buffer)
     (kill-buffer)))

(defun eimap-sentinel (conn state)
  (with-current-buffer (process-buffer conn)
    (eimap-close-1 t)))

(defun eimap-set-state (new-state)
  (when (not (eq new-state eimap-state))
    (setq eimap-state new-state)
    (eimap-upcall 'connection-state (list :state new-state))))


;;;
;;; send
;;;

(defun eimap-request* (conn req &rest rest)
  "Issues `eimap-request' REQ on CONN."
  (with-current-buffer conn
    (apply #'eimap-request req rest)))

(defun* eimap-request (req &rest data &key cbdata done barrier continue)
  "Enqueue REQ to the server.

DONE, CONTINUE, and BARRIER are callbacks with arguments (REQDATA
CBDATA); the value returned from the callback becomes the next
CBDATA.

DONE is called when the server completes the request.

BARRIER is called before the request is executed, and at the same
time BARRIER ensures that this request is not issued in parallel
with any previous request, but does not prevent following
requests from being issued in parallel.

CONTINUE is called when the server prompts for a continuation;
probably you don't want to use this."
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
    (setq data (plist-put data :tag tag))
    (setq data (plist-put data :req-str req-str))
    (setq data (plist-put data :cont-strs cont-strs))
    (setq tag-data (cons tag data))
    ;; add to request queue and kick it
    (add-to-list 'eimap-req-queue data t)
    (eimap-process-req-queue)))

(defun eimap-new-tag ()
  "Generate new IMAP tag"
  (setq eimap-tag (1+ eimap-tag))
  (concat "e" (number-to-string eimap-tag)))

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

(defun eimap-network-send (output)
  "Send IMAP data.  Must be called with the process buffer
active."
  ;; XXX this reorders multiple overlays at point
  (let ((outovr (make-overlay (1- (point)) (point))))
    (overlay-put outovr 'after-string
                 (propertize output 'face '(:foreground "red")))
    (process-send-string (get-buffer-process (current-buffer)) output)))


;;;
;;; receive
;;;

(defun eimap-network-recv (process output)
  "Process incoming network data, either line based replies or a
defined amount of octets."
  (with-current-buffer (process-buffer process)
    (save-excursion
      (goto-char (process-mark process))
      (let ((inhibit-read-only t))
        (insert output))
      (set-marker (process-mark process) (point)))
    (goto-char eimap-recv-mark)
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

(defun eimap-process-reply ()
  "Parse network reply and run protocol state machine."
  (let* ((reply (eimap-parse))
         (type (plist-get reply :type))
         (params (plist-get reply :params)))
    (setq eimap-recv-mark (point))
    (case type
      ('continue
       (eimap-process-continue-reply params))
      ('data
       (eimap-process-data-reply params))
      ('tag
       (eimap-process-tag-reply params)))))

(defun eimap-process-continue-reply (data)
  "Handle a continue reply from the server."
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
       (setq data (funcall continue data cbdata))
       (setcdr tag-record (plist-put tag-data :cbdata cbdata))))))

(defun eimap-process-common-continue (data)
  "Send literal after server prompted us for continuation."
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

(defun eimap-process-data-reply (data)
  "Handle an untagged data reply from the server."
  (let ((method (plist-get data :method))
        (resp-code (plist-get data :resp-code)))
    (when (or (eq resp-code 'CAPABILITY)
              (eq method 'CAPABILITY))
      (setq eimap-capabilities (plist-get data :capabilities)
            eimap-auth-methods (plist-get data :auth)))
    (when resp-code
      (eimap-upcall resp-code data))
    (when (eq method 'cond-state)
      (case (plist-get data :state)
        ('PREAUTH
         (when (eq eimap-state 'connected)
           (eimap-set-state 'authenticated)))
        ('BYE
         (message "server closing connection: %s" (plist-get data :text)))))
    (eimap-upcall method data)))

(defun eimap-process-tag-reply (data)
  "Handle a tagged reply from the server."
  (let* ((tag (plist-get data :tag))
         (tag-record (assoc-string tag eimap-outstanding-tags))
         (tag-data (cdr tag-record))
         (done-handler (plist-get tag-data :done))
         (cbdata (plist-get tag-data :cbdata))
         (resp-code (plist-get data :resp-code)))
    (setq eimap-outstanding-tags (delq tag-record eimap-outstanding-tags))
    (eimap-process-req-queue)
    (when resp-code
      (eimap-upcall resp-code data))
    (when done-handler
      (funcall done-handler data cbdata))))

(defun eimap-upcall (method data)
  "Report data to the application."
  (when eimap-upcall
    (funcall eimap-upcall eimap-upcall-data method data)))

(provide 'eimap-connection)
