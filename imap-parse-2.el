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
 ((response (or response-data
                response-tagged))


;;; responses

  (response-tagged `:tag tag SP resp-cond-state CRLF)
  (response-data "*" SP (or resp-cond-state
                            resp-cond-bye
                            mailbox-data
                            message-data
                            capability-data) CRLF)
  (resp-cond-bye `:bye "BYE" SP resp-text)


;;; response status and code

  (resp-cond-state `:state (or `"OK" `"NO" `"BAD") SP resp-text)
  (resp-text (opt resp-text-code) text)
  (resp-text-code "["
                  `:resp-code
                  (list
                   (or `"ALERT"
                       (`"BADCHARSET" (list (opt SP "(" astring (* SP astring) ")")))
                       capability-data
                       `"PARSE"
                       (`"PERMANENTFLAGS" SP flag-list)
                       `"READ-ONLY"
                       `"READ-WRITE"
                       (`"UIDNEXT" SP number)
                       (`"UIDVALIDITY" SP number)
                       (`"UNSEEN" SP number)
                       (atom (opt SP `:code-text (substring (+ ((not "]") (any))))))))
                  "]" SP)

  (capability-data `"CAPABILITY" (list (+ SP (or (`:auth "AUTH=" atom)
                                                 atom))))

  (flag-list "(" (list (opt flag (* SP flag))) ")")
  (flag (or ("\\Answered" `:answered)
            ("\\Flagged" `:flagged)
            ("\\Deleted" `:deleted)
            ("\\Seen" `:seen)
            ("\\Draft" `:draft)
            ("\\Recent" `:recent)
            (`:keyword atom)
            flag-extension))
  (flag-extension "\\" `:extflag atom)


;;; mailbox

  (mailbox-data (or (`"FLAGS" SP flag-list)
                    (`"LIST" SP mailbox-list)
                    (`"LSUB" SP mailbox-list)
                    (`"SEARCH" (list (* SP number)))
                    (`"STATUS" SP mailbox SP "(" status-att-list ")")
                    (`:exists-count number SP "EXISTS")
                    (`:recent-count number SP "RECENT")))

  (mailbox-list "(" (opt mbx-list-flags) ")" SP
                `:mboxsep (or ("\"" (substring QUOTED-CHAR)
                               `(str -- (eimap/parse-unquote-string))
                               "\"")
                              =nil)
                SP mailbox)
  (mailbox `:mailbox astring)
  (mbx-list-flags mbx-list-flag (* SP mbx-list-flag))
  (mbx-list-flag (or ("\\Noinferiors" `:noinferiors)
                     ("\\Noselect" `:noselect)
                     ("\\Marked" `:marked)
                     ("\\Unmarked" `:unmarked)
                     flag-extension))

  (status-att-list (list status-att-pair (* SP status-att-pair)))
  (status-att-pair (or `"MESSAGES"
                       `"RECENT"
                       `"UIDNEXT"
                       `"UIDVALIDITY"
                       `"UNSEEN")
                   SP number)


;;; message data
  (message-data number SP
                (or `"EXPUNGE"
                    (`"FETCH" SP msg-att)))
  (msg-att "(" (list (or msg-att-dynamic
                         msg-att-static)
                     (* SP (or msg-att-dynamic
                               msg-att-static)))
           ")")
  (msg-att-dynamic `:flags "FLAGS" SP flag-list)
  (msg-att-static (or (`"ENVELOPE" SP envelope)
                      (`"INTERNALDATE" SP quoted)
                      (`"RFC822" SP nstring)
                      (`"RFC822.HEADER" SP nstring)
                      (`"RFC822.TEXT" SP nstring)
                      (`"RFC822.SIZE" SP number)
                      (`"BODY" SP body)
                      (`"BODYSTRUCTURE" SP body)
                      (`"BODY" section (opt "<" number ">") SP nstring)
                      (`"UID" SP number)
                      ))
  (section "[" (opt section-spec) "]")
  (section-spec (or section-msgtext
                    (section-part (opt "." section-text))))
  (section-part `:part (list number (* "." number)))
  (section-msgtext (or `"HEADER"
                       (`"HEADER.FIELDS" (opt `".NOT") SP header-list)
                       "TEXT"))
  (section-text (or section-msgtext
                    `"MIME"))
  (header-list `:header-list "(" (list astring (* SP astring)) ")")

  (envelope "(" `:envelope
            (list
             `:date nstring SP
             `:subject nstring SP
             `:from env-addr-list SP
             `:sender env-addr-list SP
             `:reply-to env-addr-list SP
             `:to env-addr-list SP
             `:cc env-addr-list SP
             `:bcc env-addr-list SP
             `:in-reply-to nstring SP
             `:message-id nstring)
            ")")
  (env-addr-list (or ("(" (list (+ address) ")"))
                     =nil))
  (address "("
           (list
            `:name nstring SP
            `:adl nstring SP
            `:mailbox nstring SP
            `:host nstring)
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
                  mime-type SP body-fields SP `:lines number)

  (body-type-msg "XXX")
  (body-type-basic "XXX")

  (mime-type `:mime (list quoted SP quoted))
  (body-fields body-fld-param SP
               body-fld-id SP
               body-fld-desc SP
               body-fld-enc SP
               body-fld-octets)
  (body-fld-param `:param (or ("(" string-pair-list ")")
                              =nil))
  (body-fld-id `:id nstring)
  (body-fld-desc `:desc nstring)
  (body-fld-enc `:enc string)
  (body-fld-octets `:octets number)



;;; data extraction

  (string-pair-list (list string-pair (* SP string-pair)))
  (string-pair string SP string `(a b -- (cons a b)))

  (=nil "NIL" `(-- nil))

  (text `:text (substring (+ TEXT-CHAR)))
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
  (QUOTED-CHAR (or ((not quoted-specials) TEXT-CHAR)
                   ("\\" quoted-specials)))
  )
 'reverse)
