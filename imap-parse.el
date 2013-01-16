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

(parser-define
 'imap
 (parser-compile
  (define
    (/token SP " " null)
    (/token CRLF "\u000D\u000A" null)
    (/token UNTAGGED "*" nil)
    (/token OK "OK")
    (/token NO "NO")
    (/token BAD "BAD")
    (/token LBRACK "\\[" null)
    (/token RBRACK "]" null)
    (/token ATOM "[^]\x00-\x1f\x7f){ %*\"\\]+")
    (/token text "[^\x0d\x0a]+")
    (/token literal "{\\([0-9]+\\)}\x0d\x0a" (0 1) eimap/slurp-literal)
    (/token resp-text-code "[^]]+")	; hack
    (/production resp-text
                 (/and LBRACK resp-text-code RBRACK SP)
                 text)
    (/production resp-cond-state
                 (/or OK NO BAD)
                 SP
                 resp-text
		 CRLF
		 )
    (/production response-data
		 (UNTAGGED
		  SP
		  (/term foo)
		  resp-cond-state
		  ))
    )

  /and UNTAGGED SP OK SP literal SP ATOM CRLF))
