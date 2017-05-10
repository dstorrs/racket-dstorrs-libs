#lang at-exp racket

;; List of functions:
;; *) ->string   : general purpose "convert stuff to string"
;; *) 12hr->24hr : for time displays
;; *) append-file
;; *) dir-and-filename : split-path without the third return value
;; *) hash->immutable : convert an (im)mutable hash to an immutable one
;; *) hash->mutable   : convert an (im)mutable hash to a mutable one
;; *) not-null?       : what it says on the tin
;; *) pad-digits : convert, e.g. "9" to "09"
;; *) path-string->string and path-string->path
;; *) perl-true? and perl-false? : Relaxed boolean checks
;; *) px : alias for pregexp
;; *) rand-val : get a random string value, optionally with
;;     prefix. e.g: (rand-val) or (rand-val "employee-id")
;; *) running-file-dir: get the dir to the running file
;; *) running-file-path: get the complete path to the running file
;; *) safe-hash-remove : does hash-remove or hash-remove! as needed.  Returns the hash.
;; *) safe-hash-set : does hash-set or hash-set! as needed. Returns the hash.
;; *) say : macro that uses 'displayln' to output all
;;     args. e.g.: (say "num cows: " 7 ", and geese: " 8)
;; *) symbol->keyword
;; *) symbol-string->string and symbol-string->symbol
;; *) true? : opposite of false? (useful for coercing to boolean)
;; *) verify-struct  : test correctness of just parts of a structure

;;----------------------------------------------------------------------

(define/contract (->string x)
  (-> any/c string?)
  (cond ((list?   x)   (apply string-append (map ->string x)))
        ((vector?   x) (->string (vector->list x)))
        (else (~a x))))

;;----------------------------------------------------------------------

;;    Turn a 12-hour time (4pm) into a 24-hour time (16).  By default
;;    returns as number but you can ask for it as string.
(define/contract (12hr->24hr tm pm [as-str #f])
  (->* ((or/c string? exact-integer?) boolean?)
       (boolean?)
       (or/c string? exact-integer?))
  (define t (if (string? tm) (string->number tm) tm))
  (define res (+ t (if pm 12 0)))
  (if as-str (->string res) res))

;;----------------------------------------------------------------------

(define/contract (append-file source dest)
  (-> path-string? path-string? exact-positive-integer?)

  ;;    Append file, return number of bytes in file afterwards so that
  ;;    we could verify the append if so desired.
  ;;
  ;; @@TODO: This reads the entire source file into RAM, so will work
  ;; poorly on large files.  Should add a file-size check and make it
  ;; do the transfer in a loop if it's too big.
  (with-output-to-file
    dest
    #:mode 'binary
    #:exists 'append
    (thunk
     (with-input-from-file
       source
       (thunk
        (display (port->bytes))))))

  (file-size dest)
  )

;;----------------------------------------------------------------------

(define (hash->immutable h)
  (if (immutable? h)
      h
      (apply hash (flatten (for/list ((k (hash-keys h)))
                             (cons k (hash-ref h k)))))))

;;----------------------------------------------------------------------

(define (hash->mutable h)
  (if (not (immutable? h))
      h
      (make-hash (for/list ((k (hash-keys h)))
                   (cons k (hash-ref h k))))))

;;----------------------------------------------------------------------

(define/contract (not-null? lst)
  (-> list? boolean?)
  (not (null? lst)))

;;----------------------------------------------------------------------


;;    This is intended for things like turning 9 into "09" for use in
;;    dates, filenames, etc.
(define (pad-digits d [width 2] [pad "0"])
  (~a d #:left-pad-string pad #:min-width width #:align 'right))

;;----------------------------------------------------------------------

(define/contract (path-string->string p #:dir [is-dir? #f])
  (->* (path-string?) (#:dir boolean?) string?)
  ;; This will return a string corresponding to a path as
  ;; Racket would interpret it.  If you pass #:dir #t then
  ;; it will ensure the trailing path separator is on it.
  (define appender  (if is-dir? path->directory-path identity))
  (define converter (if (string? p) string->path identity))
  (path->string (appender (converter p))))

;;----------------------------------------------------------------------

(define/contract (path-string->path   p #:dir [is-dir? #f])
  (->* (path-string?) (#:dir boolean?) path?)
  (string->path (path-string->string p #:dir is-dir?)))

;;----------------------------------------------------------------------

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

;;----------------------------------------------------------------------

(define px pregexp)

;;----------------------------------------------------------------------

;;    generate a random value; generally useful in testing
;; (rand-val)                           => e.g. "203428"
;; (rand-val "car-type")                => e.g. "car-type-73038"
;; (rand-val "car-type" #:post string->symbol) => e.g. 'car-type-53084
;; (rand-val "x"
;;     #:post (lambda (s) (list s 'foo)))      => e.g. '("x-53084" foo)
;;
(define/contract (rand-val [prefix #f] #:post [converter-proc identity])
  (->* () ((or/c path? symbol? string? char? number? list? vector?)
           #:post (-> string? any))
       any) ;; generally returns string but converter-proc can change that
  (converter-proc
   (~a (if prefix
           (string-append (->string prefix) "-")
           "")
       (->string (random 1000000)))))

;;----------------------------------------------------------------------

(define/contract (safe-hash-remove h k)
  (-> hash? any/c hash?)
  (if (immutable? h)
      (hash-remove h k)
      (begin (hash-remove! h k) h)))

;;----------------------------------------------------------------------

(define/contract (safe-hash-set h k v)
  (-> hash? any/c any/c hash?)
  (if (immutable? h)
      (hash-set h k v)
      (begin (hash-set! h k v) h)))

;;----------------------------------------------------------------------

(define-syntax (say stx)
  (syntax-case stx ()
    [(_ a b ...)
     #'(displayln (~a a b ...))]))

;;----------------------------------------------------------------------

(define symbol->keyword (compose string->keyword symbol->string))

;;----------------------------------------------------------------------

(define/contract (symbol-string->string x)
  (-> (or/c symbol? string?) string?)
  (if (string? x) x (symbol->string x)))

(define/contract (symbol-string->symbol x)
  (-> (or/c symbol? string?) symbol?)
  (if (string? x) (string->symbol x) x))

;;----------------------------------------------------------------------

;;    Useful for coercing values to boolean for, e.g., inserting into DB
(define (true? x) (not (false? x)))

;;----------------------------------------------------------------------

(define/contract (verify-struct #:struct    s
                                #:type      [is-type? identity]
                                #:funcs     funcs
                                #:expected  expected)
  (->* (#:struct any/c #:funcs (listof procedure?) #:expected (or/c any/c (listof any/c)))
       (#:type (-> any/c boolean?))
       boolean?)

  (when (and (list? expected)
             (not (equal? (length funcs) (length expected))))
    (raise-arguments-error 'verify-struct
                           "funcs list and expected list must be the same length"
                           "funcs" (length funcs)
                           "expected"  (length expected)))

  (and (is-type? s)
       (cond [(list? expected) (for/and ((f funcs)
                                         (val expected))
                                 (equal? (f s) val))]
             [else (for/and ((f funcs))
                     (equal? (f s) (f expected)))]))
  )

;;----------------------------------------------------------------------

(define/contract (dir-and-filename fp)
  (-> path-string? (values path? path?))
  (define-values (d f is-dir) (split-path fp))
  (when (equal? d 'relative)
    (raise-arguments-error  'dir-and-filename
                           "Cannot accept single-element relative paths"
                           "path" (path-string->string fp)))
  (when (false? d)
    (raise-arguments-error  'dir-and-filename
                            "Cannot accept root path (/)"
                            "path" (path-string->string fp)))
  
  (define convert (if is-dir path->directory-path identity))
  (values d (convert f)))

;;----------------------------------------------------------------------

(define (running-file-path)
  (path->complete-path (find-system-path 'run-file)))

;;----------------------------------------------------------------------

(define (running-file-dir)
  (path-only (path->complete-path (find-system-path 'run-file))))

;;----------------------------------------------------------------------

(provide (all-defined-out))
