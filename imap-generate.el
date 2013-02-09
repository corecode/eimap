(require 'peg)

(defmacro eimap-defgen (funname &rest rules)
  (declare (indent 1))
  `(defun ,funname (data)
     ;(peg-generate data ,@rules)
     ,(let ((peg-creating-generator t)
            (peg-generator-data 'data))
        (peg-translate-rules rules))))

(eval-when-compile
  (eimap-defgen eimap-gen
   (command tag SP :method (or command-any
                               command-auth
                               command-nonauth
                               command-select) CRLF)

   (command-any (or '"CAPABILITY"
                    '"LOGOUT"
                    '"NOOP"))
   (command-auth (or append create delete
                     examine list lsub
                     rename select status
                     subscribe unsubscribe))
   (command-nonauth (or ;; login not supported
                     authenticate
                     ;; no STARTTLS - handled by connection layer
                     ))
   (command-select (or '"CHECK"
                       '"CLOSE"
                       '"EXPUNGE"
                       copy
                       fetch
                       store
                       uid
                       search))

   (append '"APPEND" SP :mailbox mailbox (opt SP flag-list) (opt SP date-time) SP literal)
   (create '"CREATE" SP :mailbox mailbox)
   (delete '"DELETE" SP :mailbox mailbox)
   (examine '"EXAMINE" SP :mailbox mailbox)
   (list '"LIST" SP :mailbox mailbox SP :list-mailbox list-mailbox)
   (lsub '"LSUB" SP :mailbox mailbox SP :list-mailbox list-mailbox)
   (rename '"RENAME" SP :from-mailbox mailbox SP :to-mailbox mailbox)
   (select '"SELECT" SP :mailbox mailbox)
   (status '"STATUS" SP :mailbox mailbox SP
           :status-att (list "(" status-att (* SP status-att) ")"))
   (subscribe '"SUBSCRIBE" SP :mailbox mailbox)
   (unsubscribe '"UNSUBSCRIBE" SP :mailbox mailbox)

   (authenticate '"AUTHENTICATE" SP :auth-type atom
                 (opt SP :auth-token
                      (or (substring "=") ; allow single pad
                          base64)))

   (copy '"COPY" SP :sequence-set sequence-set SP :mailbox mailbox)
   (fetch '"FETCH" SP :sequence-set sequence-set SP
          :fetch-att (or '"ALL"
                         '"FULL"
                         '"FAST"
                         fetch-att
                         (list "(" fetch-att (* SP fetch-att) ")")))
   (store '"STORE" SP :sequence-set sequence-set SP :flags store-att-flags)
   (uid :uid 't "UID" SP
        (or copy
            fetch
            search
            store))

   (search '"SEARCH" (opt SP "CHARSET" SP :charset astring)
           :keys (list (+ SP search-key)))
   (search-key (or
                (and :all 't "ALL")
                (and :answered search-key-bool-prefix "ANSWERED")
                (and :bcc "BCC" SP astring)
                (and :before "BEFORE" SP date)
                (and :body "BODY" SP astring)
                (and :cc "CC" SP astring)
                (and :deleted search-key-bool-prefix "DELETED")
                (and :flagged search-key-bool-prefix "FLAGGED")
                (and :from "FROM" SP astring)
                (and :keyword (cons search-key-bool-prefix "KEYWORD" atom))
                (and :new (or (and 't "NEW")
                              (and 'nil "OLD")))
                (and :on "ON" SP date)
                (and :recent 't "RECENT")
                (and :seen search-key-bool-prefix "SEEN")
                (and :since "SINCE" SP date)
                (and :subject "SUBJECT" SP astring)
                (and :text "TEXT" SP astring)
                (and :to "TO" SP astring)
                (and :draft search-key-bool-prefix "DRAFT")
                (and :header "HEADER" SP (cons astring SP astring))
                (and :larger "LARGER" SP number)
                (and :not "NOT" SP (list search-key))
                (and :or "OR" SP (cons search-key search-key))
                (and :sentbefore "SENTBEFORE" SP date)
                (and :senton "SENTON" SP date)
                (and :sentsince "SENTSINCE" SP date)
                (and :smaller "SMALLER" SP number)
                (and :uid SP sequence-set)
                (and :message-id sequence-set)
                (and "(" (list search-key) ")")
                ))
   (search-key-bool-prefix (or 't
                               (and 'nil "UN")))

   (flag-list "(" (list (opt flag (* SP flag))) ")")
   (mailbox astring)
   (status-att (or '"MESSAGES"
                   '"RECENT"
                   '"UIDNEXT"
                   '"UIDVALIDITY"
                   '"UNSEEN"))
   (sequence-set (list (or seq-number
                           seq-range)
                       (* "," (or seq-number
                                  seq-range))))
   (seq-range (cons seq-number ":" seq-number))
   (seq-number (or nz-number
                   '"*"))

   (fetch-att (or '"ENVELOPE"
                  '"FLAGS"
                  '"INTERNALDATE"
                  '"RFC822"
                  '"RFC822.HEADER"
                  '"RFC822.SIZE"
                  '"RFC822.TEXT"
                  '"BODY"
                  '"BODYSTRUCTURE"
                  '"UID"
                  (cons '"BODY" section)
                  (cons '"BODY.PEEK" section)))

   (section "[" (opt :section (list section-spec)) "]")
   (section-spec (or section-msgtext
                     (and :part section-part (opt "." section-text))))
   (section-part number (* "." number))
   (section-msgtext :text
                    (or '"HEADER"
                        (and (or '"HEADER.FIELDS"
                                 '"HEADER.FIELDS.NOT")
                             SP :header-list header-list)
                        '"TEXT"))
   (section-text (or section-msgtext
                     :text '"MIME"))
   (header-list "(" (list astring (* SP astring)) ")")

   (store-att-flags (opt (or '"+"
                             '"-")) "FLAGS" SP "(" (opt flag (* SP flag)) ")")
   (flag (or ;; '"\\Answered"
          ;; '"\\Flagged"
          ;; '"\\Deleted"
          ;; '"\\Seen"
          ;; '"\\Draft"
          ;; '"\\Recent"
          atom
          flag-extension))
   (flag-extension (substring "\\" atom))


;;; data generation

   (=nil 'nil "NIL")

   (text :text (substring (* TEXT-CHAR)))
   (atom (substring (+ ATOM-CHAR)))
   (tag :tag (substring (+ (not "+") ASTRING-CHAR)))
   (astring (or (substring (+ ASTRING-CHAR))
                string))
   (string (or quoted
               literal))
   (nstring (or string
                =nil))
   (quoted "\"" `(str -- (eimap-gen-quote-string str)) (substring (* QUOTED-CHAR)) "\"")
   (literal (fail))

   (number `(n -- (number-to-string n)) (substring (+ [0-9])))
   (nz-number `(n -- (number-to-string n)) (substring [1-9] (* [0-9])))

   (base64 (substring (* 4base64-char) (opt base64-terminal)))
   (base64-terminal (or (and base64-char base64-char "==")
                        (and base64-char base64-char base64-char "=")))
   (4base64-char base64-char base64-char base64-char base64-char)
   (base64-char [a-z A-Z 0-9 "+/"])

   (date quoted)                        ; XXX
   (date-time quoted)                   ; XXX
   (list-mailbox (or (+ list-char)
                     string))

;;; literals, common with parse

   (SP " ")
   (CRLF "\x0d\x0a")
   (resp-specials ["]"])
   (list-wildcards ["%*"])
   (list-char (or ATOM-CHAR
                  list-wildcards
                  resp-specials))
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
                    (and "\\" quoted-specials)))))

(eimap-gen '(:tag "c0" :method AUTHENTICATE :auth-type "PLAIN" :auth-token "FOo="))
;(eimap-gen (list :tag "c0"))p
;;; (:method SEARCH :keys (:from "foo" :not (:from "foobar")))
;;; (:method SEARCH :keys ((FROM . "foo") (NOT . (FROM . "foobar"))))
