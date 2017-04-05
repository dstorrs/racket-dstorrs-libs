#lang racket

;;    syntax->keyword and struct/kw were lifted from:
;;
;; http://www.greghendershott.com/2015/07/keyword-structs-revisited.html

(require (for-syntax racket/syntax
                     syntax/parse)
         dstorrs/list-utils
         )

(provide struct/kw hash->struct/kw)

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
                   [((ctor-arg ...) ...) #'(field.ctor-arg ...)]) ;i.e. append*
       #'(begin
           (struct struct-id (field.id ...) opt ...)
           (define (ctor-id ctor-arg ... ...) ;i.e. append*
             (struct-id field.id ...))))]))

;;----------------------------------------------------------------------

(define/contract (hash->struct/kw struct-ctor h [keys #f] #:remap-keys [remapped-keys (hash)])
  (->* (procedure? (hash/c symbol? any/c))
       ((non-empty-listof any/c) #:remap-keys (hash/c symbol? symbol?))
       struct?)

  (define sorted-keys (sort (if keys keys (hash-keys h)) symbol<?))
  (define final-keys
    (for/list ((k sorted-keys))
      (hash-ref remapped-keys k k)))

  (keyword-apply struct-ctor
                 (symbols->keywords final-keys)
                 (map (curry hash-ref h) sorted-keys)
                 '()))
