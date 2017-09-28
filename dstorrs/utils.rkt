#lang at-exp racket

;; Parameters
;; *) prefix-for-say : A string that will be prepended to all 'say' calls. Default: ""
;;
;; List of functions:
;; *) __FILE__, __LINE__, __WHERE__: current filepath/line/string of both
;; *) __FILE:__, __WHERE:__: same as previous but with ": " appended
;; *) ->string   : general purpose "convert stuff to string"
;; *) !=   : variadic. args are numbers. checks whether to see if at least one is different
;; *) 12hr->24hr : for time displays
;; *) always-return : returns a variadic function which always returns a constant value
;; *) always-true : variadic function that always returns #t. Useful for with-handlers
;; *) append-file
;; *) dir-and-filename : split-path without the third return value
;; *) directory-empty? : does the directory exist and contain nothing?
;; *) empty-string?   : is something the empty string?
;; *) ensure-directory-exists : directory will exist or this will throw
;; *) hash-key-exists? : alias for hash-has-key? because I always forget the name
;; *) hash-keys->strings : take a hash where keys are symbols or strings, make them strings
;; *) hash-keys->symbols : take a hash where keys are symbols or strings, make them symbols
;; *) hash->immutable : convert an (im)mutable hash to an immutable one
;; *) hash->mutable   : convert an (im)mutable hash to a mutable one
;; *) hash-rename-key : change, e.g., key 'name to be 'first-name
;; *) mutable-hash    : creates a mutable hash using the convenient syntax of (hash)
;; *) not-equal?      : what it says on the tin
;; *) not-null?       : is something the null list?
;; *) pad-digits : convert, e.g. "9" to "09"
;; *) path-string->string and path-string->path
;; *) perl-true? and perl-false? : Relaxed boolean checks
;; *) px : alias for pregexp
;; *) rand-val : get a random string value, optionally with
;;     prefix. e.g: (rand-val) or (rand-val "employee-id")
;; *) running-file-dir: get the dir to the running file
;; *) running-file-path: get the complete path to the running file
;; *) safe-build-path : build-path, but ignores "", #f, or 'relative
;; *) safe-hash-remove : does hash-remove or hash-remove! as needed.  Returns the hash.
;; *) safe-hash-set : does hash-set or hash-set! as needed. Returns the hash.
;; *) say : macro that uses 'displayln' to output all
;;     args. e.g.: (say "num cows: " 7 ", and geese: " 8)
;; *) silence : eliminates all data sent to current-output-port (print, display, etc)
;; *) symbol->keyword
;; *) symbol-string->string and symbol-string->symbol
;; *) true? : opposite of false? (useful for coercing to boolean)
;; *) verify-struct  : test correctness of just parts of a structure
;; *) with-temp-file : creates a temp file, ensures it will be deleted
;; on exception. THIS IS NOT REENTRANT. ONCE YOU LEAVE THE FUNCTION,
;; THE FILE IS GONE. DO NOT TRY TO, E.G. SAVE A CONTINUATION FROM
;; INSIDE THE FUNCTION, OR USE IT WITH FUNCTIONS THAT
;; DO. (E.G. SEND/SUSPEND FROM THE WEBSERVER)

;;----------------------------------------------------------------------

(define-syntax (__LINE__ stx)
  (with-syntax ((line (syntax-line stx)))
    (syntax-case stx ()
      [_ #'line])))

;;----------------------------------------------------------------------

(define-syntax (__FILE__ stx)
  (with-syntax ((this-file (syntax-source stx)))
    (syntax-case stx ()
      [_ #'this-file])))

(define-syntax (__FILE:__ stx) ; Note the ':'
  (with-syntax ((this-file (syntax-source stx)))
    (syntax-case stx ()
      [_ #'(~a this-file ": ")])))

;;----------------------------------------------------------------------

(define-syntax (__WHERE__ stx)
  (with-syntax ((fpath (syntax-source stx))
                (line  (syntax-line stx)))
    (syntax-case stx ()
      [_ #'(~a "file:" fpath " (line:" line ")")])))

(define-syntax (__WHERE:__ stx) ; Note the ':'
  (with-syntax ((fpath (syntax-source stx))
                (line  (syntax-line stx)))
    (syntax-case stx ()
      [_ #'(~a "file:" fpath " (line:" line "): ")])))

;;----------------------------------------------------------------------

(define/contract (->string x)
  (-> any/c string?)
  (cond ((list?   x)   (apply string-append (map ->string x)))
        ((vector?   x) (->string (vector->list x)))
        (else (~a x))))

;;----------------------------------------------------------------------

(define/contract (!= . args)
  (->* () () #:rest (non-empty-listof number?) boolean?)
  (not (andmap (curry equal? (first args)) args)))

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

(define (always-return val)
  (define (result . args) val)
  result)

(define always-true (always-return #t))
(define match-anything (always-return #t))

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

(define hash-key-exists? hash-has-key?) ; just as alias because I always forget the name


;;----------------------------------------------------------------------

(define/contract (hash-keys->strings h)
  (-> hash? hash?)
  ((if (immutable? h) identity hash->mutable)
   (for/hash ([(k v) h])
     (values (->string k) v))))

(define/contract (hash-keys->symbols h)
  (-> hash? hash?)
  ((if (immutable? h) identity hash->mutable)
   (for/hash ([(k v) h])
     (values (if (symbol? k) k (string->symbol (~a k)))
             v))))

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

(define (mutable-hash . args)
  (hash->mutable (apply hash args)))

;;----------------------------------------------------------------------

(define/contract (not-equal? x y)
  (-> any/c any/c boolean?)
  (not (equal? x y)))

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

(define/contract (path-string->string p #:dir? [is-dir? #f] #:reject-empty-string [reject-empty-string #f])
  (->* ((or/c "" path-string?)) (#:dir? boolean? #:reject-empty-string boolean?) string?)
  ;; This will return a string corresponding to a path as
  ;; Racket would interpret it.  If you pass #:dir? #t then
  ;; it will ensure the trailing path separator is on it.
  ;;
  ;; As a special case, if you pass "" (empty string) then
  ;; it will return "" instead of throwing an exception as
  ;; path->string would do.  You can disable this behavior
  ;; by setting #:reject-empty-string to #t

  (call/cc
   (lambda (return)
     (cond [(and (equal? p "")
                 (not reject-empty-string))
            (return "")]
           [else
            (define appender  (if is-dir? path->directory-path identity))
            (define converter (if (string? p) string->path identity))
            (path->string (appender (converter p)))]))))

;;----------------------------------------------------------------------

(define/contract (path-string->path   p #:dir? [is-dir? #f])
  (->* (path-string?) (#:dir? boolean?) path?)
  (string->path (path-string->string p #:dir? is-dir?)))

;;----------------------------------------------------------------------

;;    Because the Racket concept of booleans is inflexible.
;;    Things that are perl-false:
;;        #f, "", '(), #<void>, anything matching (zero?),
;;        and any string that equals 0
(define (perl-true? x) (not (perl-false? x)))
(define (perl-false? x)
  (cond
    [(string? x) (or (with-handlers ((exn:fail? (lambda (e) #f)))
                       (= 0 (string->number x)))
                     (= 0 (string-length x)))]
    [(number? x) (zero? x)]
    [(list?   x) (null? x)]
    [else (or (void? x)
              (false? x))]))

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

(define/contract (safe-hash-remove h . keys)
  (->* (hash?) () #:rest (non-empty-listof any/c) hash?)
  (define is-imm (immutable? h))
  (for/fold ((hsh h))
            ((k keys))
    (if is-imm
        (hash-remove hsh k)
        (begin (hash-remove! hsh k) h))))

;;----------------------------------------------------------------------

(define/contract (safe-hash-set h . args)
  (->* (hash?)
       ()
       #:rest (and/c list?
                     (λ (lst)
                       (let ([len (length lst)])
                         (and (even? len)
                              (not (= 0 len))))))
       hash?)

  (define args-hash (apply hash args))
  (define is-imm (immutable? h))
  (for/fold ((hsh h))
            ((k (hash-keys args-hash)))
    (if is-imm
        (hash-set hsh k (hash-ref args-hash k))
        (begin (hash-set! hsh k (hash-ref args-hash k)) h))))

;;----------------------------------------------------------------------

(define prefix-for-say (make-parameter ""))
(define-syntax (say stx)
  (syntax-case stx ()
    [(say a ...)
     #'(displayln (~a (prefix-for-say) a ...))]
    ))

;;----------------------------------------------------------------------

(define-syntax (silence stx)
  (syntax-case stx ()
    [(silence body0 body1 ...)
     #'(parameterize ((current-output-port (open-output-nowhere)))
         body0 body1 ...)]))

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

(define/contract (dir-and-filename fp #:as-str? [as-str #f])
  (->* (path-string?) (#:as-str? boolean?) (values path-string? path-string?))

  (define-values (d f is-dir) (split-path fp))
  (cond [(equal? d 'relative)
         (raise-arguments-error  'dir-and-filename
                                 "Cannot accept single-element relative paths"
                                 "path" (path-string->string fp))]
        [(false? d)
         (raise-arguments-error  'dir-and-filename
                                 "Cannot accept root path (/)"
                                 "path" (path-string->string fp))]
        [else
         (define convert (compose (if as-str path-string->string identity)
                                  (if is-dir path->directory-path identity)))
         (values (convert d) (convert f))]))

;;----------------------------------------------------------------------

(define/contract (directory-empty? dir)
  (-> path-string? boolean?)
  ;;    All we need to know is if there is SOMETHING in
  ;;    the directory.  Using directory-list would be
  ;;    overkill and could be slow on heavily populated
  ;;    directories.
  (call/cc
   (lambda (return)
     (parameterize ([current-directory dir])
       (for ((p (in-directory)))
         (return #f))
       (return #t)))))

;;----------------------------------------------------------------------

(define/contract (empty-string? x)
  (-> any/c boolean?)
  (and (string? x) (equal? x "")))

;;----------------------------------------------------------------------

(define/contract (ensure-directory-exists dir)
  (-> path-string? boolean?)
  (or (directory-exists? dir)
      (and (make-directory* dir) #f) ; make-directory* returns void, which is true, but we need to continue
      (directory-exists? dir)
      (raise (exn:fail:filesystem (~a "directory does not exist and could not be created: " dir) (current-continuation-marks)))))

;;----------------------------------------------------------------------

(define (running-file-path)
  (path->complete-path (find-system-path 'run-file)))

;;----------------------------------------------------------------------

(define (running-file-dir)
  (path-only (path->complete-path (find-system-path 'run-file))))

;;----------------------------------------------------------------------

(define/contract (safe-build-path #:as-str [as-str #f] . args)
  (->* ()
       (#:as-str boolean?)
       #:rest (listof (or/c #f 'relative "" path-string?))
       path-string?)

  ((if as-str path->string identity)
   (apply build-path (filter (negate (or/c "" #f 'relative)) args))))

;;----------------------------------------------------------------------

(define/contract (with-temp-file proc)
  (-> (-> path? any) any)
  (define the-path (make-temporary-file))
  (dynamic-wind
    (thunk #t)
    (thunk (proc the-path))
    (thunk (delete-file the-path))))

;;----------------------------------------------------------------------

(define/contract (hash-rename-key h old-key new-key)
  (-> hash? any/c any/c hash?)

  (when (not (hash-has-key? h old-key))
    (raise-arguments-error "no such key"
                           "old-key" old-key
                           "new-key" new-key
                           "hash" h))

  (safe-hash-remove
   (safe-hash-set h new-key (hash-ref h old-key))
   old-key))

;;----------------------------------------------------------------------


(provide (all-defined-out))
