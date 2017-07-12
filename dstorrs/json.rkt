#lang racket

(require json
         dstorrs/try
         )

(provide (all-defined-out))

(define/contract (valid-json? j)
  (-> any/c boolean?)
  (cond [(not (jsexpr? j)) #f]
        [(string? j) (try [(not (false? (string->jsexpr j)))]
                          [catch (identity (lambda (e) #f))])]
        [else #t]))
