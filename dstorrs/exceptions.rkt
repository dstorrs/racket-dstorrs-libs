#lang racket

(define/contract (create-exn ctor msg)
  (-> procedure? string? exn?)
  (ctor msg (current-continuation-marks)))

(define/contract (create/raise-exn ctor msg)
  (-> procedure? string? void?)
  (raise (create-exn ctor msg)))
                  
(provide (all-defined-out))
