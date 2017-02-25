#lang racket

(require json
         dstorrs/utils
         dstorrs/try
         json
         )

(provide (all-defined-out))

(define/contract (valid-json? j)
  (-> any/c boolean?)
  (cond [(not (jsexpr? j)) #f]
        [(string? j) (try [(true? (string->jsexpr j))]
                          [catch (identity (lambda (e) #f))])]
        [else #t]))
