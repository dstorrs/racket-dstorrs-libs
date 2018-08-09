#lang racket/base

(provide L
         alist->hash
         )

(define L list)

;;----------------------------------------------------------------------

(define alist->hash make-immutable-hash)

