#lang racket

;; List of functions:
;; *) px : alias for pregexp
;; *) path-string->string and path-string->path
;; *) symbol-string->string and symbol-string->symbol
;; *) true? : opposite of false? (not usually needed)
;; *) append-file
;; *) say : macro that uses 'displayln' to output all
;;     args. e.g.: (say "num cows: " 7 ", and geese: " 8)
;; *) rand-val : get a random string value, optionally with 
;;     prefix. e.g: (rand-val) or (rand-val "employee-id")
;; *) perl-true? and perl-false? : Relaxed boolean checks
;; *) pad-digits : convert, e.g. "9" to "09"
;; *) 12hr->24hr : for time displays

(define px pregexp)

(define (path-string->string p) (if (path? p)   (path->string p) p))
(define (path-string->path   p) (if (path? p) p (string->path p)))

(define (symbol-string->string x) (if (string? x) x (symbol->string x)))
(define (symbol-string->symbol x) (if (string? x) (string->symbol x) x))

;;    not sure when you'd need this, but it itched at me
(define (true? x) (not (false? x))) 

(define/contract (append-file source dest)
  (-> path-string? path-string? exact-positive-integer?)

  ;;    Append file, return number of bytes in file afterwards so that we could verify the append if so
  (with-output-to-file dest
    #:mode 'binary
    #:exists 'append
    (thunk
     (with-input-from-file
       source
       (thunk
        (display (port->bytes))))))

  (file-size dest)
  )
        
(define-syntax (say stx)
  (syntax-case stx ()
	[(_ a b ...)
	 #'(displayln (~a a b ...))]))

;;    generate a random value; generally useful in testing
;; (rand-val)                           => e.g. "203428"
;; (rand-val "car-type")                => e.g. "car-type-73038"
;; (rand-val "car-type" #:post string->symbol) => e.g. 'car-type-53084
;; (rand-val "x"
;;     #:post (lambda (s) (list s 'foo)))      => e.g. '("x-53084" foo)
;;
(define/contract (rand-val [prefix #f] #:post [converter-proc identity])
  (->* () (string? #:post (-> string? any)) any) ;; generally returns string
  (converter-proc
   (~a (if prefix
		   (string-append prefix "-")
		   "")
	   (number->string (random 1000000)))))

;;    Because the Racket concept of booleans is inflexible.
;;    Things that are perl-false:
;;        #f, "", '(), #<void>, and anything matching (zero?)
(define (perl-true? x) (not (perl-false? x)))
(define (perl-false? x)
  (cond
   ((string? x) (= 0 (string-length x)))
   ((number? x) (zero? x))
   ((list?   x) (null? x))
   (else (or (void? x)
			 (false? x)))))

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
