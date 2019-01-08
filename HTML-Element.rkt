#lang racket/base

(require html-parsing
         net/url
         racket/contract/base
         racket/contract/region
         racket/format
         (only-in racket/function negate)
         racket/list
         racket/match
         racket/string
         sxml
         "list-utils.rkt"
         "try.rkt"
         (except-in  "utils.rkt" hash->keyword-apply) ; already required from list-utils.rkt
         "web.rkt")

(provide (all-defined-out)
         (all-from-out sxml))


(define (xexp? x)
  (-> any/c boolean?)
  (and (list? x) (not-null? x) (equal? (first x) '*TOP*)))

(define attr-list? (ntype?? '@))

;;--------------------------------------------------
;;    attr-val:  xexp 'key [default value]  -> any/c
(define/contract (attr-val x key [def #f])
  (->* (list/not-null? symbol?) (any/c) any/c)

  (hash-ref (attr-hash x) key def))

;;--------------------------------------------------
;;    (html-element-from src) -> xexpr
;;        source : input-port?, path-string?, url-string?, or string
;;
;;  If given a string source it will try to work out what the thing is
;;  -- a path to an HTML file, a URL to a web page, or some actual
;;  HTML.
;;
(define/contract (html-element-from src)
  (-> (or/c url? path? input-port? string? xexp?) xexp?)

  (define is-url  #px"^https?://")
  (define is-html #px"^<(!DOCTYPE|[A-Za-z]+)")

  (define (from-file)
    (when (not (file-exists? src))
      (raise-arguments-error 'html-element-from "argument looks like path but not to existing file" "arg" src))

    (define p (open-input-file src))
    (try [(html->xexp p)]
         [catch (match-anything raise)]
         [finally (close-input-port p)]))

  (cond [(xexp? src)  src]
        [(port? src)  (html->xexp src)]
        [(path? src)  (from-file)]
        [(url?  src)  (web/call src)]
        [(regexp-match is-url  src) (web/call src)]
        [(regexp-match is-html src)  (html->xexp (open-input-string src))]
        [else  (from-file)]))

;;--------------------------------------------------
;;    Take an xexp, a hash, and a quoted symbol (e.g. 'class) and
;;    check if that symbol is an attribute of the xexp.  If not,
;;    return #f.  If so, return the value of that hash entry.
;;
(define (has-attr? html-expr key [val #f])
  (define h (attr-hash html-expr))
  (define v (hash-ref h key #f))
  (if val
      (and (regexp-match
            (pregexp (string-append "\\b" val "\\b"))
            val)
           v)
      v))



;;--------------------------------------------------
;;    (attr-hash el) -> immutable hash
;;    el : list representing a tag. (anything else will return empty hash) 
;;
;;    Given a list representing it pulls the attributes (if any) out and converts
;;    them to a hash of attribute-name => attribute-value.  If there
;;    are no attributes, returns an empty hash.
;;
;;    It expects to be given a proper xexp (that is, a tag, then the
;;    attributes, then the content), but if given only the attributes
;;    it will still work.
;
;;    Example:  '(div (@ (class "footer")) "This is the footer")
;;    returns:  #hash((class . "footer"))
;;
;;    Example:  '(@ (class "footer"))
;;    returns:  #hash((class . "footer"))
;;
(define (attr-hash el)
  (match el
    [(list '@ (list keys vals) ...)  (for/hash ([k keys][v vals])
                                       (values k
                                               (match v
                                                 [(? string?) (string-trim v)]
                                                 [_ v])))]
    [(list tag)  (hash)]
    [(list tag (and attrs (list '@ (list k v) ...)) _ ...)  (attr-hash attrs)]
    [_ (hash)]
    ))


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
;;    Looks through an xexpr for something that meets criteria
;;    specified by the #:match function and the optional #:attr
;;    keyword.  It runs the 'action' procedure on anything that
;;    matches and returns a list of results, or a string if requested.
;;
;;    @@IMPORTANT: the results of the 'action' function will be
;;    autoboxed to ensure it returns a list
;;
;;    action        ->  optional, function to call on matched values.  Defaults to 'list'
;;    #:first-only? ->  optional, boolean. #t means only return the first item instead of list
;;    #:match       ->  optional, function returning boolean
;;    #:tag         ->  optional, a symbol representing an HTML tag
;;    #:attr        ->  optional, quoted-value   : attribute that must be present (e.g. 'class)
;;                      or dotted pair : '(class . "container")
;;                      or dotted pair : '(id    . #rx"foo.+bar")
;;                      or dotted pair : '(src   . #px"foo.+bar")
;;
;;    When the #:attr is a dotted pair, the specified key must be
;;    present for the match to succeed.  If the value is a string it
;;    must match exactly.  If it is a regexp or pregexp then the match
;;    must succeed.
(define (look-down some-content
                   [action list]                  ;; default: list all matches
                   #:match       [:match-func #f] ;; default: match everything
                   #:tag         [:tag #f]        ;; ditto
                   #:attr        [:attr #f]       ;; ditto
                   #:first-only? [first-only? #f] ;; only return the first item found
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
  (define result
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
  (cond [(null? result) result]
        [first-only?    (first result)]
        [else           result]))


;;----------------------------------------------------------------------

(define block-level-elements
  (make-immutable-hash
   (append
    (for/list ([i (in-range-inc 1 6)])
      (cons (string->symbol (~a "h" i)) #t))
    '((address    . #t)
      (article    . #t)
      (aside      . #t)
      (blockquote . #t)
      (canvas     . #t)
      (dd         . #t)
      (div        . #t)
      (dl         . #t)
      (dt         . #t)
      (fieldset   . #t)
      (figcaption . #t)
      (figure     . #t)
      (footer     . #t)
      (form       . #t)
      (header     . #t)
      (hgroup     . #t)
      (hr         . #t)
      (li         . #t)
      (main       . #t)
      (nav        . #t)
      (noscript   . #t)
      (ol         . #t)
      (output     . #t)
      (p          . #t)
      (pre        . #t)
      (section    . #t)
      (table      . #t)
      (tfoot      . #t)
      (ul         . #t)
      (video      . #t)))))
