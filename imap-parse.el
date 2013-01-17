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
  (if (listp args)
      (mapcar 'eimap/parse-unqote-string args)
    (lexical-let
	((qstr (parser-extract-string args)))
      (replace-regexp-in-string "\\\\" "" qstr))))

(defun eimap/parse-number (args)
  (if (listp args)
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

    (/token nz-number "[1-9][0-9]*" 0 eimap/parse-number)
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
		      ((/token SEARCH) (/or (/greedy (SP nz-number)) /always-match))
		      ((/token STATUS) SP mailbox SP LPAREN status-att-list RPAREN)
		      ((/production EXISTS
				    number SP "EXISTS")
		       (/production RECENT
				    number SP "RECENT"))))

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
		 nz-number SP (/or (/token EXPUNGE)
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
		      ((/token UIDNEXT) SP nz-number)
		      ((/token UIDVALIDITY) SP nz-number)
		      ((/token UNSEEN) SP nz-number)
		      (atom (/or (SP (/token resp-text-code-text "[^]]+" 0 parser-token-string)) /always-match)))
		 RBRACK SP)

    (/production resp-text
                 (/or resp-text-code /always-match) text)
    (/production resp-cond-state
                 (/or (/token OK)
		      (/token NO)
		      (/token BAD)) SP resp-text CRLF)
    (/production response-data
                 ((/token untagged "*") SP
		  (/or resp-cond-state
		       resp-cond-bye
		       mailbox-data
		       message-data
		       capability-data)))
    (/production resp-cond-bye
		 (/token BYE) SP resp-text)
    (/production response-tagged
		 tag SP resp-cond-state)
    )

  /and response-data))
