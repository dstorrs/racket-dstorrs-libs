#lang at-exp racket

(provide (all-defined-out))

(define/contract (create-exn ctor msg . args)
  (->* (procedure? string?) () #:rest (listof any/c) exn?)
  (apply ctor (append (list msg (current-continuation-marks)) args)))



(define/contract (create/raise-exn ctor msg . args)
  (->* (procedure? string?) () #:rest (listof any/c) any)
  (raise (apply create-exn (append (list ctor msg) args))))

(define/contract (verify-arg arg-name arg-val pred source [pred-name #f])
  (->* (string? any/c (-> any/c boolean?) symbol?)
       (string?)
       any)
  ;; There doesn't seem to be a simple way to write a contract on
  ;; lambdas.  You can use define/contract for named functions but not
  ;; for, e.g., the #:guard clause on a struct.  This is a poor man's
  ;; version of contracts.  You specify the name of the variable
  ;; you're checking, its value, the predicate it must satisfy, and
  ;; the name of the function you're calling it from.  Optionally, you
  ;; can also specify the name of the predicate if what you're passing
  ;; is a raw lambda instead of a named function.
  ;;
  ;; The return value is incidental and should not be relied on.  It
  ;; either works or it throws using raise-arguments-error.
  ;;
  ;; Examples that pass:
  ;;    (verify-arg "requested-space" 7 exact-positive-integer? 'has-sufficient-space)
  ;;    (verify-arg "next-piece" 'apple (or/c 'apple 'pear) 'check-fruit-type)
  ;;
  ;; Examples that throw:
  ;;    (verify-arg "requested-space" 'bob exact-positive-integer? 'has-sufficient-space)
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
  
  (unless (pred arg-val)
    (raise-arguments-error
     source
     @~a{'@arg-name' must be @(if pred-name pred-name (object-name pred))}
     arg-name arg-val)))
