#lang racket/base

(require (for-syntax racket/base)
         json
         racket/contract/base
         racket/contract/region
         racket/function
         racket/string
         "try.rkt")

(provide valid-json?
         (all-from-out json))

;;======================================================================
;;    See the Racket-standard 'json' module for more.
;;======================================================================

;;    Verify that some JSON is valid.  For strings it does
;;    string->jsexpr and (if decoding succeeds) returns the result.
;;    If decoding fails then valid-json? returns #f
(define/contract (valid-json? j)
  (-> any/c (or/c #f jsexpr?))

  (cond [(not (jsexpr? j)) #f]
        [(non-empty-string? j)
         (try [ (define result (string->jsexpr j))
                (displayln result)
                (cond [(eof-object? result) #f]
                      [else result])
                ]
              [catch (identity (lambda (e) #f))])]
        [else #t]))
