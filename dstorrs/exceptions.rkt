#lang at-exp racket

(provide (all-defined-out))

(define/contract (create-exn ctor msg . args)
  (->* (procedure? string?) () #:rest (listof any/c) exn?)
  (apply ctor (append (list msg (current-continuation-marks)) args)))



(define/contract (create/raise-exn ctor msg . args)
  (->* (procedure? string?) () #:rest (listof any/c) any)
  (raise (apply create-exn (append (list ctor msg) args))))
