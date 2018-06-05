#lang racket

;;    syntax->keyword and struct/kw were lifted from:
;;
;; http://www.greghendershott.com/2015/07/keyword-structs-revisited.html

(require (for-syntax racket/syntax
                     syntax/parse)
         "list-utils.rkt"
         )

(provide struct/kw
         hash->struct/kw
         verify-struct
         ;make-functional-setter
         )

;;; Example usage:
;;
;; Define a struct type
;; (struct/kw foo (a b [c 42]) #:transparent)
;;
;; Use normal ctor
;; (foo 1 2 3)                ; => (foo 1 2 3)
;;
;; Use keyword ctor
;; (foo/kw #:a 1 #:b 2 #:c 3) ; => (foo 1 2 3)
;;
;; Use keyword ctor, taking advantage of default arg for #:c field
;; (foo/kw #:a 1 #:b 2)       ; => (foo 1 2 42)
;;
;; Use a hash to create the struct
;; (hash->struct/kw foo/kw (hash 'a 1 'b 2 'c 3)) ; => (foo/kw #:a 1 #:b 2 #:c 3)
;;
;; Use a hash that has more keys than you need:
;; (hash->struct/kw foo/kw (hash 'a 1 'b 2 'c 3 'd 5 'e 8)
;;                         '(a b c)) ; => (foo/kw #:a 1 #:b 2 #:c 3)
;;
;; Use a hash and rename some of the keys
;; (hash->struct/kw foo/kw (hash 'a 1 'b 2 'charlie 3)
;;                         #:remap-keys (hash 'charlie 'c)) ; => (foo/kw #:a 1 #:b 2 #:c 3)
;;
;; Use a hash, only some of the keys, and rename some of the keys
;; (hash->struct/kw foo/kw (hash 'a 1 'b 2 'charlie 3 'd 5 'e 8)
;;                         '(a b charlie)
;;                         #:remap-keys (hash 'charlie 'c)) ; => (foo/kw #:a 1 #:b 2 #:c 3)

;;----------------------------------------------------------------------

(begin-for-syntax
  (define syntax->keyword (compose1 string->keyword symbol->string syntax->datum)))

(define-syntax (struct/kw stx)
  (define-syntax-class field
    (pattern id:id
             #:with ctor-arg #`(#,(syntax->keyword #'id) id))
    (pattern [id:id default:expr]
             #:with ctor-arg #`(#,(syntax->keyword #'id) [id default])))
  (syntax-parse stx
    [(_ struct-id:id (field:field ...) opt ...)
     (with-syntax ([ctor-id (format-id #'struct-id "~a/kw" #'struct-id)]
                   [((ctor-arg ...) ...) #'(field.ctor-arg ...)]) ; The double ... flattens one level
       #'(begin
           (struct struct-id (field.id ...) opt ...)
           (define (ctor-id ctor-arg ... ...) ; The double ... flattens one level
             (struct-id field.id ...))))]
    ;
    [(_ struct-id:id super-id:id (field:field ...) opt ...)
     (with-syntax ([ctor-id (format-id #'struct-id "~a/kw" #'struct-id)]
                   [((ctor-arg ...) ...) #'(field.ctor-arg ...)]) ; The double ... flattens one level
       #'(begin
           (struct struct-id super-id (field.id ...) opt ...)
           (define (ctor-id ctor-arg ... ...) ; The double ... flattens one level
             (struct-id field.id ...))))]
    ))

;;----------------------------------------------------------------------

;;   (hash->struct/kw struct-ctor h [restricted-keys #f] #:remap-keys [remapped-keys (hash)])
;;
;; Takes a struct constructor function and a hash, returns a struct of
;; the appropriate type.  The ctor must be one that accepts keywords
;; (e.g. created by struct/kw) and the keywords must case-sensitively
;; match the keys of the hash.  If the hash contains keys that should
;; not be used then you can pass the list of keys that SHOULD be used
;; and all other will be ignored.  You can also pass a second hash
;; (the 'remap hash') that maps the keys from the first hash (the
;; 'data hash') to a new form that will be used for the keyword.  For
;; example, perhaps the keys of your data hash include 'person-name
;; but the corresponding element of your structure is 'employee-name.
;; You could then pass a remap hash like (hash 'person-name
;; 'employee-name) to describe the desired change.
;;
(define/contract (hash->struct/kw struct-ctor h [restricted-keys #f] #:remap-keys [remapped-keys (hash)])
  (->* (procedure? (hash/c symbol? any/c))
       ((non-empty-listof any/c) #:remap-keys (hash/c symbol? symbol?))
       struct?)

  (define keys-used (if restricted-keys restricted-keys (hash-keys h)))
  (define mapping
    (for/hash ((k keys-used))
      (values (hash-ref remapped-keys k k)
              (hash-ref h k))))

  (define sorted-keys (sort (hash-keys mapping) symbol<?))
  (keyword-apply struct-ctor
                 (symbols->keywords sorted-keys)
                 (map (curry hash-ref mapping) sorted-keys)
                 '()))

;;----------------------------------------------------------------------

;; (verify-struct #:struct    s                   ; the struct to verify
;;                #:type      [is-type? identity] ; a predicate that must return true
;;                #:funcs     [funcs '()]         ; list of functions to test
;;                #:expected  [expected '()])     ; required return value of corresponding func
;;
;; Given a struct, verify that it meets certain criteria. The 'funcs'
;; and 'expected' list must be the same length. If you let all
;; optional arguments default then it will return #t, but that's a
;; little silly.
(define/contract (verify-struct #:struct     s
                                #:type       [is-type? identity]
                                #:predicates [predicates '()]
                                #:funcs      [funcs '()]
                                #:expected   [expected '()])
  (->* (#:struct any/c)
       (#:type (-> any/c boolean?)
        #:predicates (listof (-> any/c any))
        #:funcs (listof procedure?)
        #:expected (or/c any/c (listof any/c)))
       boolean?)

  (when (and (list? expected)
             (not (equal? (length funcs) (length expected))))
    (raise-arguments-error 'verify-struct
                           "'funcs' list and 'expected' list must be the same length"
                           "length funcs"     (length funcs)
                           "length expected"  (length expected)))

  (and (is-type? s)
       (for/and ([p predicates])
         (p s))
       (cond [(list? expected) (for/and ((f funcs)
                                         (val expected))
                                 (equal? (f s) val))]
             [else (for/and ((f funcs))
                     (equal? (f s) (f expected)))])))

;;----------------------------------------------------------------------

;;     make-functional-setter: macro for generating non-mutating field
;;     setter functions for a struct
;;
;; Define a struct:  (struct book (title current-page filepath) #:transparent)
;;
;; Generate 'set-book-title', 'set-book-current-page', and 'set-book-filepath'.
;; All of these take two arguments: the 'book' struct to update and the new value.
;;    (make-functional-setter book title)
;;    (make-functional-setter book current-page  exact-positive-integer?)
;;    (make-functional-setter book filepath      path-string?            path->string)
;;
;; Details:
;;    set-book-title           accepts any value, regardless of sensibility
;;    set-book-current-page    accepts only exact-positive-integer?s, else contract violation
;;    set-book-filepath        accepts only path-string?s, converts to string before storing
;;
;; Examples:
;;    (define b (book "Foundation" 297 "/foo/bar"))
;;    b                                                ; (book "Foundation" 297 "/foo/bar")
;;    (set-book-title b (hash))                        ; (book (hash) 297 "/foo/bar")
;;    (set-book-current-page b 99)                     ; (book "Foundation" 99 "/foo/bar")
;;    (set-book-current-page b 'x)                     ; ERROR!  Contract violation
;;    (set-book-filepath b (build-path "/foo"))        ; (book "Foundation" 297 "/foo")
;;
;; (define-syntax (make-functional-setter stx)
;;   (syntax-parse stx
;;     ; First, grab the name of the struct and the field we're making
;;     ; this for.  We'll build some stuff here then re-parse instead of
;;     ; copy/pasting for every pattern match
;;     [(_ type-name field-name ignored ...)
;;      (with-syntax* ([func-name   (format-id #'type-name "set-~a-~a" #'type-name #'field-name)]
;;                     [func-header #'(func-name the-struct val)]
;;                     [definer     #'define]
;;                     [type-pred   (format-id #'type-pred "~a?" #'type-name)]
;;                     [func-body   #'(struct-copy type-name the-struct [field-name val])]
;;                     )
;;        (syntax-parse stx
;;          [(_ _ _) #'(definer func-header func-body)]
;;          [(_ _ _ field-contract:expr ignored ...)
;;           (with-syntax ([definer #'define/contract]
;;                         [func-contract #'(-> type-pred field-contract type-pred)])
;;             (syntax-parse stx
;;               [(_ _ _ _) #'(definer func-header func-contract func-body)]
;;               [(_ _ _ _ wrapper:expr)
;;                #'(definer func-header
;;                    func-contract
;;                    (struct-copy type-name the-struct [field-name (wrapper val)]))]))]))]))
