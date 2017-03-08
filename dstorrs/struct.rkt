#lang racket

;;    syntax->keyword and struct/kw were lifted from:
;;
;; http://www.greghendershott.com/2015/07/keyword-structs-revisited.html

(require (for-syntax racket/syntax
                     syntax/parse)
         dstorrs/list-utils
         )

(provide (all-defined-out))

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


(define/contract (hash->struct/kw h struct-ctor [keys #f] #:remap-keys [remapped-keys (hash)])
  (->* ((hash/c symbol? any/c) procedure?)
       ((non-empty-listof any/c) #:remap-keys (hash/c symbol? symbol?))
       any/c) ; there's no clean way to say 'a struct'

  (define sorted-keys (sort (if keys keys (hash-keys h)) symbol<?))
  (define final-keys
    (for/list ((k sorted-keys))
      (hash-ref remapped-keys k k)))

  (keyword-apply struct-ctor
                 (symbols->keywords final-keys)
                 (map (curry hash-ref h) sorted-keys)
                 '()))
