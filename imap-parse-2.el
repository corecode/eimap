(require 'peg)

(defmacro peg-defparse (funname rules &optional transform)
  (let ((parser (peg-translate-rules rules)))
    `(defun ,funname ()
                                        ;,(pp parser)
       (let ((result ,parser)
             (transform ,transform))
         (cond
          ((not transform) result)
          ((symbolp transform) (funcall transform result))
          (t (error "Invalid transform %S" transform)))))))


(defun eimap/parse-unquote-string (str)
  (replace-regexp-in-string "\\\\\\(.\\)" "\\1" str))

(peg-add-method normalize literalstr ()
  `(literalstr))

(peg-add-method translate literalstr ()
  `(when (looking-at "{\\([0-9]+\\)}\x0d\x0a")
     (let* ((litlen (string-to-number (match-string 1)))
            (litstart (match-end 0))
            (litend (+ litstart litlen)))
       (when (>= (point-max) litend)
         (goto-char litstart)
         ,(peg-translate-exp '(action (push (point) peg-stack)))
         (goto-char litend)
         ,(peg-translate-exp '(action
                               (push (buffer-substring-no-properties (pop peg-stack)
                                                                     (point)) peg-stack)))))))

(peg-add-method detect-cycles literalstr (path) nil)
(peg-add-method merge-error literalstr (merged)
  "IMAP literal")

(peg-defparse
 eimap/parse
 (

;;; responses

  (response (or continue-req
                response-data
                response-tagged))

  (continue-req (cons 'type 'continue)
                "+" SP resp-text CRLF)

  (response-tagged (cons 'type 'tag)
                   (cons 'tag tag) SP resp-cond-state CRLF)

  (response-data (cons 'type 'data)
                 "*" SP
                 (or resp-cond-state
                     mailbox-data
                     message-data
                     capability-data) CRLF)


;;; response status and code

  (resp-cond-state (cons 'state (or '"OK"
                                    '"NO"
                                    '"BAD"
                                    '"BYE"
                                    '"PREAUTH")) SP resp-text)
  (resp-text (opt resp-text-code) text)
  (resp-text-code "["
                  (cons
                   'resp-code
                   (or
                    '"ALERT"
                    '"PARSE"
                    '"READ-ONLY"
                    '"READ-WRITE"
                    (cons '"BADCHARSET"
                          (list (opt SP "(" astring
                                     (* SP astring) ")")))
                    capability-data
                    (cons '"PERMANENTFLAGS" (and SP flag-list))
                    (cons '"UIDNEXT" SP number)
                    (cons '"UIDVALIDITY" SP number)
                    (cons '"UNSEEN" SP number)
                    (cons atom (opt SP (substring (+ (and (not "]")
                                                          (any))))))))
                  "]" SP)

  (capability-data (cons '"CAPABILITY"
                         (list (+ SP ;; (or
                                  ;;  (and "AUTH="
                                  ;;       (cons 'AUTH atom)))
                                  atom))))

  (flag-list "(" (list (opt flag (* SP flag))) ")")
  (flag (or ;; '"\\Answered"
         ;; '"\\Flagged"
         ;; '"\\Deleted"
         ;; '"\\Seen"
         ;; '"\\Draft"
         ;; '"\\Recent"
         atom
         flag-extension))
  (flag-extension "\\" atom `(s -- (concat "\\" s)))


;;; mailbox

  (mailbox-data (cons 'data 'mailbox)
                (or (cons '"FLAGS" SP flag-list)
                    (cons '"LIST" SP mailbox-list)
                    (cons '"LSUB" SP mailbox-list)
                    (cons '"SEARCH" (list (* SP number)))
                    (cons '"STATUS" SP mailbox SP "(" status-att-list ")")
                    (cons 'EXISTS number SP "EXISTS")
                    (cons 'RECENT number SP "RECENT")))

  (mailbox-list "(" (opt (cons 'flags mbx-list-flags)) ")" SP
                (cons'mboxsep (or (and "\"" (substring QUOTED-CHAR)
                                       `(str -- (eimap/parse-unquote-string))
                                       "\"")
                                  =nil))
                SP mailbox)

  (mailbox (cons 'mailbox astring))
  (mbx-list-flags (list mbx-list-flag (* SP mbx-list-flag)))
  (mbx-list-flag (or ;; '"\\Noinferiors"
                  ;; '"\\Noselect"
                  ;; '"\\Marked"
                  ;; '"\\Unmarked"
                  flag-extension))

  (status-att-list (cons 'att
                         (list status-att-pair (* SP status-att-pair))))
  (status-att-pair (cons (or '"MESSAGES"
                             '"RECENT"
                             '"UIDNEXT"
                             '"UIDVALIDITY"
                             '"UNSEEN")
                         SP number))


;;; message data
  (message-data (cons 'data 'message)
                (cons 'msgid number) SP
                (or (cons 'action'"EXPUNGE")
                    (and (cons 'action'"FETCH") SP msg-att)))
  (msg-att "(" (or msg-att-dynamic
                   msg-att-static)
           (* SP (or msg-att-dynamic
                     msg-att-static))
           ")")
  (msg-att-dynamic (cons '"FLAGS" SP flag-list))
  (msg-att-static (or (cons '"ENVELOPE" SP envelope)
                      (cons '"INTERNALDATE" SP quoted)
                      (cons '"RFC822" SP nstring)
                      (cons '"RFC822.HEADER" SP nstring)
                      (cons '"RFC822.TEXT" SP nstring)
                      (cons '"RFC822.SIZE" SP number)
                      (cons '"BODY" SP body)
                      (cons '"BODYSTRUCTURE" SP body)
                      (and (cons 'bodysection "BODY" section)
                           (opt (cons 'offset  "<" number ">")) SP
                           (cons 'data nstring))
                      (cons '"UID" SP number)
                      ))
  (section "[" (opt section-spec) "]")
  (section-spec (or section-msgtext
                    (and section-part (opt "." section-text))))
  (section-part number (* "." number))
  (section-msgtext (or '"HEADER"
                       (and (cons (or '"HEADER.FIELDS"
                                      '"HEADER.FIELDS.NOT")
                                  SP header-list))
                       "TEXT"))
  (section-text (or section-msgtext
                    '"MIME"))
  (header-list "(" astring (* SP astring) ")")

  (envelope "("
            (cons 'date nstring SP)
            (cons 'subject nstring SP)
            (cons 'from env-addr-list SP)
            (cons 'sender env-addr-list SP)
            (cons 'reply-to env-addr-list SP)
            (cons 'to env-addr-list SP)
            (cons 'cc env-addr-list SP)
            (cons 'bcc env-addr-list SP)
            (cons 'in-reply-to nstring SP)
            (cons 'message-id nstring)
            ")")
  (env-addr-list (or (and "(" (+ address) ")")
                     =nil))
  (address "("
           (list
            (cons 'name nstring SP)
            (cons 'adl nstring SP)
            (cons 'mailbox nstring SP)
            (cons 'host nstring))
           ")")

;;; body

  (body "(" (or body-type-1part
                body-type-mpart) ")")
  (body-type-1part (or body-type-text
                                        ;body-type-msg
                                        ;body-type-basic
                       )
                                        ;  (opt SP body-ext-1part)
                   )
  (body-ext-1part "XXX")

  (body-type-mpart "XXX")

  (body-type-text (if "\"TEXT\"" SP)
                  mime-type SP body-fields SP (cons 'lines number))

  (body-type-msg "XXX")
  (body-type-basic "XXX")

  (mime-type (cons 'mime quoted SP quoted))
  (body-fields body-fld-param SP
               body-fld-id SP
               body-fld-desc SP
               body-fld-enc SP
               body-fld-octets)
  (body-fld-param (cons 'param (or (and "(" string-pair-list ")")
                                   =nil)))
  (body-fld-id (cons 'id nstring))
  (body-fld-desc (cons 'desc nstring))
  (body-fld-enc (cons 'enc string))
  (body-fld-octets (cons 'octets number))



;;; data extraction

  (string-pair-list string-pair (* SP string-pair))
  (string-pair (cons string SP string))

  (=nil "NIL" `(-- nil))

  (text (cons 'text (substring (+ TEXT-CHAR))))
  (atom (substring (+ ATOM-CHAR)))
  (tag (substring (+ (not "+") ASTRING-CHAR)))
  (astring (or (substring (+ ASTRING-CHAR))
               string))
  (string (or quoted
              (literalstr)))
  (nstring (or string
               =nil))
  (quoted "\"" (substring (* QUOTED-CHAR)) `(str -- (eimap/parse-unquote-string str)) "\"")

  (number (substring (+ [0-9])) `(str -- (string-to-number str)))


;;; literals

  (SP " ")
  (CRLF "\x0d\x0a")
  (resp-specials ["]"])
  (list-wildcards ["%*"])
  (atom-specials (or ["(){ " ?\x7f]
                     (range 0 31)
                     list-wildcards
                     quoted-specials
                     resp-specials))
  (ATOM-CHAR (not atom-specials) (any))
  (ASTRING-CHAR (or ATOM-CHAR
                    resp-specials))
  (TEXT-CHAR (not ["\x0d\x0a"]) (any))
  (quoted-specials ["\\\""])
  (QUOTED-CHAR (or (and (not quoted-specials) TEXT-CHAR)
                   (and "\\" quoted-specials)))
  )
 'reverse)
