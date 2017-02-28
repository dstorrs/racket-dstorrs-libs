#lang racket/base

;;    syntax->keyword and struct/kw were lifted from:
;;
;; http://www.greghendershott.com/2015/07/keyword-structs-revisited.html

(require (for-syntax racket/base
                     racket/list
                     racket/syntax
                     syntax/parse))

(provide struct/kw)

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


