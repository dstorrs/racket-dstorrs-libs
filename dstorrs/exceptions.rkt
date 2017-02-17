#lang racket

(define/contract (create-exn ctor msg . args)
  (->* (procedure? string?) () #:rest (listof any/c) exn?)
  (apply ctor (append (list msg (current-continuation-marks)) args)))

(define/contract (create/raise-exn ctor msg . args)
  (->* (procedure? string?) () #:rest (listof any/c) any)
  (raise (create-exn ctor msg args)))
                  
(provide (all-defined-out))
