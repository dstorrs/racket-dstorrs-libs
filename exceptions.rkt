#lang at-exp racket/base

(require (for-syntax racket/base)
         racket/contract/base
         racket/contract/region
         racket/format
         racket/match)

(provide (struct-out exn:fail:insufficient-space)
         (struct-out exn:fail:filesystem:errno:file-not-found)
         refine-filesystem-exn
         raise/refine-filesystem-exn
         exn:fail:insufficient-space/c
         exn:fail:insufficient-space/kw
         create-exn
         create/raise-exn
         verify-arg)

;;======================================================================
;;  Functions for easy creation and management of exceptions, as well
;;  as some type definitions.
;;======================================================================

(struct exn:fail:filesystem:errno:file-not-found exn:fail:filesystem:errno ())

(define/contract (refine-filesystem-exn e)
  (-> exn:fail:filesystem? exn:fail:filesystem?)
  (cond [(not (exn:fail:filesystem:errno? e)) e]
        [else   (define msg (exn-message e))
                (define cm  (exn-continuation-marks e))
                (define errno  (exn:fail:filesystem:errno-errno e))
                (match msg
                  [(pregexp "No such file ")
                   (exn:fail:filesystem:errno:file-not-found msg cm errno)]
                  [_ e])]))

;;----------------------------------------------------------------------

(define/contract (raise/refine-filesystem-exn e)
  (-> exn:fail:filesystem? any) ; 'any' because we're going to raise, not return
  (raise (refine-filesystem-exn e)))

;;----------------------------------------------------------------------

; Contract for the exn:fail:insufficient-space exception (defined below)
(define/contract (exn:fail:insufficient-space/c msg ccm req avail source type)
  (-> any/c any/c any/c any/c any/c any/c ;; Don't check on input...
      (values ;; ...check on output. Makes for cleaner error messages.
       string? continuation-mark-set?
       exact-positive-integer?  integer? symbol?
       ))
  (values msg ccm req avail source))

;;----------------------------------------------------------------------
;; exn:fail:insufficient-space : exception for when there isn't enough
;; disk space available
;;
;;    You can build this struct directly, but you're better off using
;;    exn:fail:insufficient-space/kw. (see below)
;;
;;  Superclass fields:
;;    message  =>  string?
;;    ccm      =>  continuation-marks-set? (typically from (current-continuation-marks))
;;
;;  Class fields:
;;    requested => exact-positive-integer?
;;    available => exact-positive-integer?
;;    request-source => symbol?  (label for who made the request)
(struct exn:fail:insufficient-space
  exn:fail
  (requested available request-source)
  #:transparent
  #:guard exn:fail:insufficient-space/c)

;;    Easier way to define the above.  Defaults the ccm field
(define/contract (exn:fail:insufficient-space/kw #:requested requested
                                                 #:available available
                                                 #:request-source request-source
                                                 #:msg [msg #f])
  (->* (#:requested exact-positive-integer?
        #:available integer? ; could be negative if we over-reserved
        #:request-source symbol?)
       (#:msg string?)
       exn:fail:insufficient-space?)

  (exn:fail:insufficient-space
   (if msg
       msg
       (format "Insufficient space to satisfy request from ~a.\n\trequested space: ~a\n\tavailable space: ~a"
               request-source
               requested
               available))
   (current-continuation-marks)
   requested
   available
   request-source))

;;----------------------------------------------------------------------

;;  Creates an exception struct and returns it.  NOTE: This really needs to be
;;  a macro, as otherwise the 'current-continuation-marks mistakenly
;;  comes from here instead of from the caller.
(define (create-exn ctor msg . args)
  (->* (procedure? string?) () #:rest (listof any/c) exn?)
  (apply ctor (append (list msg (current-continuation-marks)) args)))

;;----------------------------------------------------------------------

;;  Creates an exception struct and throws it.
(define/contract (create/raise-exn ctor msg . args)
  (->* (procedure? string?) () #:rest (listof any/c) any)
  (raise (apply create-exn (append (list ctor msg) args))))

;;----------------------------------------------------------------------


;; (verify-arg arg-name arg-val pred source [pred-name #f])
;;
;; There doesn't seem to be a simple way to write a contract on
;; lambdas.  You can use define/contract for named functions but not
;; for, e.g., the #:guard clause on a struct.  This is a poor man's
;; version of contracts.  You specify the name of the variable you're
;; checking, its value, the predicate it must satisfy, and the name of
;; the function you're calling it from.  Optionally, you can also
;; specify the name of the predicate if what you're passing is a raw
;; lambda instead of a named function.  Otherwise the name will be
;; obtained via (object-name pred)
;;
;; The return value is incidental and should not be relied on.  It
;; either passes or it throws using raise-arguments-error.
;;
;; Examples that pass:
;;    (verify-arg "requested-space" 7 exact-positive-integer? 'has-sufficient-space)
;;    (verify-arg "next-piece" 'apple (or/c 'apple 'pear) 'check-fruit-type)
;;
;; Examples that throw:
;;    (verify-arg "requested-space" 'bob exact-positive-integer? 'has-sufficient-space)
;;
;;    The above throws an exn:fail:contract with this message:
;; has-sufficient-space: requested-space must be exact-positive-integer?
;;   requested-space: 'bob
;;
;;
;;    (verify-arg "next-piece" 'banana (or/c 'apple 'pear) 'check-fruit-type)
;;    The above throws an exn:fail:contract with this message:
;; check-fruit-type: next-piece must be flat-or/c
;;   next-piece: 'banana
;;
;;    (verify-arg "next-piece" 'banana (or/c 'apple 'pear) 'check-fruit-type "apple or pear")
;;    The above throws an exn:fail:contract with this message:
;; check-fruit-type: next-piece must be apple or pear
;;   next-piece: 'banana
;;
;; NOTES: First off, this probably shouldn't be here, since there has
;; to be an easier and built-in way to do this that I simply haven't
;; found yet.  Second, this probably shouldn't be in this library
;; since it's not explicitly related to exceptions.  I'm tempted to
;; put it in utils.rkt, but that's getting pretty overstuffed.
;;
(define/contract (verify-arg arg-name arg-val pred source [pred-name #f])
  (->* (string? any/c (-> any/c boolean?) symbol?)
       (string?)
       any)
  (unless (pred arg-val)
    (raise-arguments-error
     source
     @~a{'@arg-name' must be @(if pred-name pred-name (object-name pred))}
     arg-name arg-val)))
