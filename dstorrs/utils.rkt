#lang racket

(define (true? x) (not (false? x)))

(define-syntax (say stx)
  (syntax-case stx ()
	[(_ a b ...)
	 #'(displayln (~a a b ...))]))

;;    Because the Racket concept of booleans is inflexible
(define (perl-true? x) (not (perl-false? x)))
(define (perl-false? x)
  (cond
   ((string? x) (= 0 (string-length x)))
   ((number? x) (zero? x))
   ((list?   x) (null? x))
   (else (false? x))))

;;    This is intended for things like turning 9 into "09" for use in
;;    dates, filenames, etc.  
(define (pad-digits d [width 2] [pad "0"])
  (~a d #:left-pad-string pad #:min-width width #:align 'right))

;;    Turn a 12-hour time (4pm) into a 24-hour time (16).  By default
;;    returns as number but you can ask for it as string.
(define/contract (12hr->24hr tm pm [as-str #f])
  (->* ((or/c string? exact-integer?) boolean?)
	   (boolean?)
	   (or/c string? exact-integer?))
  (define t (if (string? tm) (string->number tm) tm))
  (define res (+ t (if pm 12 0)))
  (if as-str (number->string res) res))
	 

(provide (all-defined-out))