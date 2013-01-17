(defun eimap/slurp-literal (args)
  (lexical-let*
      ((match (car args))
       (lenmatch (cadr args))
       (lenstr (parser-extract-string lenmatch))
       (len (string-to-number lenstr))
       (matchstart (car match))
       (litstart (cdr match))
       (litend (+ litstart len))
       (literal (parser-extract-string (cons litstart litend))))
    ;; HACK:
    ;; set matchdata 0 so that the parser can jump over the literal
    (set-match-data (list matchstart litend))
    literal))

(defun eimap/parse-unqote-string (args)
  (if (listp (car args))
      (mapcar 'eimap/parse-unqote-string args)
    (lexical-let
	((qstr (parser-extract-string args)))
      (replace-regexp-in-string "\\\\\\(.\\)" "\\1" qstr))))

(defun eimap/parse-number (args)
  (if (listp (car args))
      (mapcar 'eimap/parse-number args)
    (lexical-let
	((str (parser-extract-string args)))
      (string-to-number str 10))))


(parser-define
 'imap
 (parser-compile
  (define
    (/token SP " " null)
    (/token CRLF "\x0d\x0a" null)

    (/token LBRACK "\\[" null)
    (/token RBRACK "]" null)
    (/token LPAREN "(" null)
    (/token RPAREN ")" null)

    (/token NIL)

    (/token number "[0-9]+" 0 eimap/parse-number)

    (/token tag "[^]\x00-\x1f\x7f){ %*\"\\]" 0 parser-token-string)

    (/token atom "[^]\x00-\x1f\x7f){ %*\"\\]+" 0 parser-token-string)
    (/token literal "{\\([0-9]+\\)}\x0d\x0a" (0 1) eimap/slurp-literal)
    (/token quoted "\"\\(\\(?:[^\x0d\x0a\\\"]*\\|\\\\[\\\"]\\)*\\)\""
	    1 eimap/parse-unqote-string)
    (/production string
		 (/or quoted
		      literal))
    (/production astring
		 (/or (/token string- "[^\x00-\x1f\x7f){ %*\"\\]+"
			      0 parser-token-string)
		      string))
    (/production nstring
		 (/or string
		      NIL))

    (/token text "[^\x0d\x0a]+" 0 parser-token-string)

    (/token flag "\\\\?[^]\x00-\x1f\x7f){ %*\"\\]+" 0 parser-token-string)
    (/production flag-list
		 LPAREN
		 (/or (flag (/or (/greedy (SP flag)) /always-match))
		      /always-match)
		 RPAREN)

    (/production hiersep
		 (/or (/token hiersep-1 "\"\\([^\x0d\x0a\\\"]\|\\[\\\"]\\)\""
			      1 eimap/parse-unqote-string)
		      NIL))
    (/production mailbox-list
		 LPAREN mbx-list-flags RPAREN SP
		 hiersep SP (/or (/token INBOX)
				 astring))

    (/production status-att-pair
		 (/or (/token MESSAGES)
		      (/token RECENT)
		      (/token UIDNEXT)
		      (/token UIDVALIDITY)
		      (/token UNSEEN))
		 SP number)
    (/production status-att-list
		 status-att-pair
		 (/or (/greedy (SP status-att-pair)) /always-match))

    (/production mailbox-data
		 (/or ((/token FLAGS) SP flag-list)
		      ((/token LIST) SP mailbox-list)
		      ((/token LSUB) SP mailbox-list)
		      ((/token SEARCH) (/or (/greedy (SP number)) /always-match))
		      ((/token STATUS) SP mailbox SP LPAREN status-att-list RPAREN)
		      ((/production exists-count
				    number SP "EXISTS")
		       (/production recent-count
				    number SP "RECENT"))))

    (/production env-addr-list
		 (/or (LPAREN (/greedy address) RPAREN)
		      NIL))
    (/alias env-bcc env-addr-list)
    (/alias env-cc env-addr-list)
    (/alias env-from env-addr-list)
    (/alias env-reply-to env-addr-list)
    (/alias env-sender env-addr-list)
    (/alias env-to env-addr-list)
    (/alias env-date nstring)
    (/alias env-subject nstring)
    (/alias env-message-id nstring)

    (/production envelope
		 LPAREN env-date SP env-subject SP env-from SP
		 env-sender SP env-reply-to SP env-to SP env-cc SP
		 env-bcc SP env-in-reply-to SP env-message-id RPAREN)
    (/alias date-time quoted)

    (/production media-message
		 DQUOTE (/token MESSAGE "MESSAGE" 0 parser-token-string) DQUOTE
		 SP DQUOTE (/token RFC822- "RFC822" 0 parser-token-string) DQUOTE)
    (/production media-text
		 DQUOTE (/token TEXT "TEXT" 0 parser-token-string) DQUOTE
		 SP string)
    (/production media-basic
		 string SP string)

    (/production body-fld-param-pair
		 string SP string)
    (/production body-fld-param
		 (/or (LPAREN
		       body-fld-param-pair
		       (/or (/greedy (SP body-fld-param-pair))
			    /always-match) RPAREN)
		      NIL))
    (/alias body-fld-id nstring)
    (/alias body-fld-desc nstring)
    (/alias body-fld-end string)
    (/alias body-fld-octets number)
    (/production body-fields
		 body-fld-param SP body-fld-id SP body-fld-desc SP
		 body-fld-enc SP body-fld-octets)
    (/alias body-fld-lines number)
    (/production body-type-msg
		 media-message SP body-fields SP envelope
		 SP body SP body-fld-lines)
    (/production body-type-text
		 media-text SP body-fields SP body-fld-lines)
    (/production body-type-basic
		 media-basic SP body-fields)
    (/alias body-fld-md5 nstring)
    (/production body-fld-dsp
		 (/or (LPAREN string SP body-fld-param RPAREN)
		      NIL))
    (/production body-fld-lang
		 (/or nstring
		      (LPAREN string (/or (/greedy (SP string))
					  /always-match) RPAREN)))
    (/alias body-fld-loc nstring)
    (/production body-extension
		 (/or nstring
		      number
		      (LPAREN body-extension (/or (/greedy body-extension)
						  /always-match) RPAREN)))
    (/production body-ext-1part
		 body-fld-md5
		 (/or (SP body-fld-dsp
			  (/or (SP body-fld-lang
				   (/or (SP body-fld-loc
					    (/or (/greedy (SP body-extension))
						 /always-match))
					/always-match))
			       /always-match))
		      /always-match))
    (/production body-type-1part
		 (/or body-type-msg
		      body-type-text
		      body-type-basic)
		 (/or (SP body-ext-1part)
		      /always-match))
    (/production body-ext-mpart
		 body-fld-param
		 (/or (SP body-fld-dsp
			  (/or (SP body-fld-lang
				   (/or (SP body-fld-loc
					    (/or (/greedy (SP body-extension))
						 /always-match))
					/always-match))
			       /always-match))
		      /always-match))
    (/production body-type-mpart
		 /greedy body SP string
		 (/or (SP body-ext-mpart)
		      /always-match))
    (/production body
		 LPAREN (/or body-type-1part
			     body-type-mpart) RPAREN)

    (/production msg-att-static
		 (/or ((/token ENVELOPE) SP envelope)
		      ((/token INTERNALDATE) SP date-time)
		      ((/token RFC822) (/or (/token .HEADER)
					    (/token .TEXT)
					    /always-match) SP nstring)
		      ((/token RFC822.SIZE) SP number)
		      ((/token BODY) (/or (/token STRUCTURE)
					  /always-match) SP body)
		      (BODY section (/or (LANGL number RANGL)
					 /always-match) SP nstring)
		      ((/token UID) SP number)))
    (/production msg-att
		 LPAREN
		 (/or msg-att-dynamic
		      msg-att-static)
		 (/or (/greedy (SP (/or msg-att-dynamic
					msg-att-static)))
		      /always-match)
		 RPAREN)

    (/production message-data
		 number SP (/or (/token EXPUNGE)
				((/token FETCH) SP msg-att)))

    (/production capability-data
		 "CAPABILITY"
		 (/greedy (SP (/or (/production AUTH "AUTH=" atom)
				   atom))))

    (/production resp-text-code
		 LBRACK
		 (/or (/token ALERT)

		      ((/token BADCHARSET) ((SP
					     LPAREN
					     astring (/or (/greedy (SP astring) /always-match))
					     RPAREN) /always-match))

		      capability-data
		      (/token PARSE)

		      ((/token PERMANENTFLAGS) (SP
						LPAREN
						(flag-perm (/or (/greedy (SP flag-perm)) /always-match))
						RPAREN))

		      (/token READ-ONLY)
		      (/token READ-WRITE)
		      (/token TRYCREATE)
		      (UIDNEXT SP number)
		      (UIDVALIDITY SP number)
		      (UNSEEN SP number)
		      (atom (/or (SP (/token resp-text-code-text "[^]]+" 0 parser-token-string)) /always-match)))
		 RBRACK SP)

    (/production resp-text
                 (/or resp-text-code /always-match) text)
    (/production resp-cond-state
                 (/or (/token OK)
		      (/token NO)
		      (/token BAD)) SP resp-text CRLF)
    (/production response-data
                 "*" SP (/or resp-cond-state
			     resp-cond-bye
			     mailbox-data
			     message-data
			     capability-data) CRLF)
    (/production resp-cond-bye
		 (/token BYE) SP resp-text)
    (/production response-tagged
		 tag SP resp-cond-state)
    (/production continue-req
		 "+" SP /or resp-text CRLF)
    (/production response
		 (/or continue-req
		      response-data
		      response-tagged)))
  response))
