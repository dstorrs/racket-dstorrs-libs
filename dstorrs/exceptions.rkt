#lang racket

(define/contract (create-exn ctor msg)
  (-> procedure? string? exn?)
  (ctor msg (current-continuation-marks)))

(provide (all-defined-out))
