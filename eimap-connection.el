(require 'cl)

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
    (set (make-local-variable 'eimap-state) :connecting)
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

(provide 'eimap-connection)
