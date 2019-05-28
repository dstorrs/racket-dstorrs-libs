#lang racket

(require net/url
		 net/url-connect
		 openssl
		 rackunit
		 racket/pretty
		 "list-utils.rkt" ; for atom?
		 "web.rkt"        ; for web/call
		 html-parsing
		 sxml
		 )

(define attr-list? (ntype?? '@))

;;--------------------------------------------------
;;    attr-val:  xexp 'key [default value]  -> any
(define (attr-val x key [def #f])
  (let ((h (attr-hash x)))
	(if (hash? h)
		(hash-ref h key def)
		def)))

;;--------------------------------------------------
;;    (html-treebuilder-new src) -> xexpr
;;        source : string (URL, HTML, or filepath)
;;
(define (html-treebuilder-new src)
  (let ([is-url  #px"^https?://"]
		[is-html #px"^<(!DOCTYPE|[A-Za-z]+)"])
	(cond [(regexp-match is-url  src) (web/call src)]
		  [(regexp-match is-html src) (html->xexp (open-input-string src))]
		  [else (let ((p (open-input-file src)))
				   (begin0 (html->xexp p)
						   (close-input-port p)))])))

;;--------------------------------------------------
;;    Take an xexp, a hash, and a quoted symbol (e.g. 'class) and
;;    check if that symbol is an attribute of the xexp.  If not,
;;    return #f.  If so, return the value of that hash entry.
;;
(define (has-attr? html-expr key [val #f])
  (define h (attr-hash html-expr))
  (define v (and (hash-has-key? h key)
				 (hash-ref h key)))
  (if val
	  (and (regexp-match
			(pregexp (string-append "\\b" val "\\b"))
			val)
		   v)
	  v))



;;--------------------------------------------------
;;    (attr-hash el) -> immutable hash
;;    el : xexp
;;
;;    Given an xexp it pulls the attributes (if any) out and converts
;;    them to a hash of attribute-name => attribute-value.  If there
;;    are no attributes, returns an empty hash.
;;
;;    Example:  (div (@ (class "footer")) "This is the footer")
;;    returns:  #hash((class . "footer"))
;;
(define (attr-hash el)
  ;;(displayln (format "### In attr-hash, el is: ~a" el))
  (define (attr-hash-helper l)
	(cond [(atom? l)  (hash)]
		  [(null? l)  (hash)]
		  [(atom? (car l))  (hash)]
		  [(< (length (car l)) 2)  (hash)]
		  [else (hash-set (attr-hash-helper (cdr l))
						  (first (car l))
						  (string-trim (second (car l))))]))
  (attr-hash-helper (sxml:attr-list el)))


;;--------------------------------------------------
;;    text-of xexp  -> string
;;
;;    Concats all strings in the element recursively
(define (text-of x)
  (string-trim
   (cond [(string? x) (string-trim x)]
		 [(atom? x)   ""]
		 [(empty? x)  ""]
		 [((ntype?? 'br) x)  "\n"]
		 [(attr-list? x)  ""]
		 [else
		  (string-join (list (text-of (car x))
							 (text-of (cdr x))))])
   #px"[\t ]+" ;; don't trim newlines
   ))
;;  (p (@ (class "hi") (style "border: 1px solid red;")) "Hello world")

;;----------------------------------------------------------------------
;;    Take an xexp that models an A tag and return a two-element list:
;;    the URL and the link text.  Note that it's called an 'hlink'
;;    (hyperlink) to disambiguate it from the 'link' and 'hyperlink'
;;    structs in Scribble.
(define (hlink-data hlink-xexp [base ""])
  (list (->absolute-url base
						(hash-ref (attr-hash hlink-xexp) 'href)) ;; URL
		(get hlink-xexp '(2))))               ;; link text

;;----------------------------------------------------------------------
;;    Take an 'A' xexp and return the URL as a string
;;
(define (hlink-url x)
  (first (hlink-data x)))

;;    Ditto, but return the link text
(define (hlink-text x)
  (second (hlink-data x)))


;;----------------------------------------------------------------------
;;    Get a list of all the hlink xexps in a document
(define (all-hlinks x) (look-down x #:tag 'a))

;;----------------------------------------------------------------------
;;    Get the xexp for the first link in a document
(define (first-hlink x)
  (let ((r (all-hlinks x)))
	(if (null? r) r (first r))))

;;----------------------------------------------------------------------
;;    look-down  ->  returns list or string
;;
;;    Looks through an xexpr that meets criteria specified by the
;;    #:match function and the optional #:attr keyword.  It runs the
;;    'action' procedure on anything that matches and returns a list
;;    of results, or a string if requested.
;;
;;    @@IMPORTANT: the 'action' function will be autoboxed to ensure
;;    it returns a list
;;
;;    action        ->  optional, function to call on matched values.  Defaults to 'list'
;;    #:match       ->  optional, function returning boolean
;;    #:tag         ->  optional, a symbol representing an HTML tag
;;    #:attr        ->  optional, quoted-value   : attribute that must be present (e.g. 'class)
;;                      or dotted pair : ('class . "container")
;;                      or dotted pair : ('class . #rx"foo.+bar")
;;                      or dotted pair : ('class . #px"foo.+bar")
;;
;;    When the #:attr is a dotted pair, the specified key must be
;;    present for the match to succeed.  If the value is a string it
;;    must match exactly.  If it is a regexp or pregexp then the match
;;    must succeed.
(define (look-down some-content
				   [action list]             ;; Default: list all matches
				   #:match [:match-func #f]  ;; Default: match everything
				   #:tag   [:tag #f]         ;; Ditto
				   #:attr  [:attr #f]        ;; Ditto
				   )
  (define (match-all x) #t)
  (define :tag? (if :tag (ntype?? :tag) match-all))
  (define :match? (if :match-func :match-func match-all))
  (define h (attr-hash some-content))
  (define (coerce-to-regexp val)
	(if (string? val)
		(pregexp (string-append "\\b" val "\\b"))
		val))
  (define (verify-attr el)
	(cond
	 [(pair? :attr)            ;; car will be key, cdr is string or regexp
	  (let* ([key (car :attr)]
			 [val (cdr :attr)]
			 [res   (has-attr? el key)])
		;;(displayln  "verify attr, regex match (el, k, v, res): ~a,~a,~a,~a" el key val res)
		(and res
			 (regexp-match (coerce-to-regexp val) res)))]
	 ;;
	 ;;    :attr can be just a name; the attribute must exist in the element
	 [:attr (has-attr? some-content :attr)]
	 ;;
	 ;; if :attr wasn't set, then validation automatically succeeds
	 [else #t]))
  (define (searched-for? el)
	;;(displayln (format "### in searched for, (tag? el) '~a', el: ~a" (:tag? el) el))
	(and (:tag? el)  ;; Use 'and' instead of 'or' so that :match? can trump
		 (verify-attr el)
		 (:match? el)))
  (define (ld c) ;; can't use curryr here since we need to use keyword args
	;;(displayln (format "ld. attr: '~a'. atom? c: ~a " :attr (atom? c)))
	(autobox
	 (if (atom? c)
		 empty
		 (look-down c action #:match searched-for? #:attr :attr))))
  ;;
  ;; NOTE: Body of 'look-down' starts here
  (cond
   [(empty?        some-content)  (begin
									;;(displayln (format  "### empty some-content"))
									empty)]
   [(attr-list?    some-content)  (begin
									;;(displayln (format  "### is attr list"))
									empty)]
   [(searched-for? some-content)  (begin
									;;(displayln (format  "### cond searched-for some-content"))
									(autobox (action some-content)))]
   [else                          (begin ;;(displayln (format "### else"))
										 (apply append (map ld some-content)))]))


;;----------------------------------------------------------------------

(provide (all-defined-out))
