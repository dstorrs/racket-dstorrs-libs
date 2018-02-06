#lang at-exp racket

(require racket/hash) ; for hash-union

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
;; *) append-file : concat one file onto another
;; *) delete-file-if-exists : if it's there, get rid of it. if it's not, shut up
;; *) dir-and-filename : split-path without the third return value
;; *) directory-empty? : does the directory exist and contain nothing?
;; *) empty-string?   : is something the empty string?
;; *) ensure-field-set : verify that a field in a struct/hash/list/etc is set
;; *) ensure-directory-exists : directory will exist or this will throw
;; *) hash-key-exists? : alias for hash-has-key? because I always forget the name
;; *) hash-keys->strings : take a hash where keys are symbols or strings, make them strings
;; *) hash-keys->symbols : take a hash where keys are symbols or strings, make them symbols
;; *) hash->immutable : convert an (im)mutable hash to an immutable one
;; *) hash->meld   : combine to or more hashes with later entries overwriting earlier ones
;; *) hash->mutable   : convert an (im)mutable hash to a mutable one
;; *) hash-rename-key : change, e.g., key 'name to be 'first-name
;; *) mutable-hash    : creates a mutable hash using the convenient syntax of (hash)
;; *) not-equal?      : what it says on the tin
;; *) not-null?       : is something the null list?
;; *) one?            : (equal? arg 1)
;; *) pad-digits : convert, e.g. "9" to "09"
;; *) path-string->string and path-string->path
;; *) perl-true? and perl-false? : Relaxed boolean checks
;; *) px : alias for pregexp
;; *) rand-val : get a random string value, optionally with
;;     prefix. e.g: (rand-val) or (rand-val "employee-id")
;; *) running-file-dir: get the dir to the running file
;; *) running-file-path: get the complete path to the running file
;; *) safe-build-path : build-path, but ignores "", #f, or 'relative
;; *) safe-file-exists? : checks if file exists but doesn't throw on bad input
;; *) safe-hash-remove : does hash-remove or hash-remove! as needed.  Returns the hash.
;; *) safe-hash-set : does hash-set or hash-set! as needed. Returns the hash.
;; *) safe-substring : like substring but won't puke if you ask for more than is available
;; *) say : macro that uses 'displayln' to output all
;;     args. e.g.: (say "num cows: " 7 ", and geese: " 8)
;; *) silence : eliminates all data sent to current-output-port (print, display, etc)
;; *) symbol->keyword
;; *) symbol-string?  : is it either a symbol or a string?
;; *) symbol-string->string and symbol-string->symbol
;; *) thunk?  ; is it a procedure of no arguments?
;; *) true? : opposite of false? (useful for coercing to boolean)
;; *) unwrap-val : call a thunk, force a promise, or return a val
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
  (-> path-string? path-string? natural-number/c)

  ;;    Append file, return number of bytes in file afterwards so that
  ;;    we could verify the append if so desired.
  (with-output-to-file dest   #:mode 'binary #:exists 'append
    (thunk
     (define size (file-size source))

     (with-input-from-file source #:mode 'binary
       (thunk
        (let loop ()
          (define data (read-bytes 1))
          (when (not (eof-object? data))
            (display data)
            (loop)))))))

  (file-size dest)
  )

;;----------------------------------------------------------------------

(define hash-key-exists? hash-has-key?) ; just as alias because I always forget the name


;;----------------------------------------------------------------------

(define/contract (hash-keys->strings h #:dash->underscore? [dash->underscore? #f])
  (->* (hash?) (#:dash->underscore? boolean?) hash?)

  ((if (immutable? h) identity hash->mutable)
   (for/hash ([(k v) h])
     (let ([key (->string k)])
       (values (if dash->underscore? (regexp-replace* #px"-" key "_") key)
               v)))))

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

(define/contract (hash->mutable h)
  (-> hash? (and/c hash? (not/c immutable?)))
  (if (not (immutable? h))
      h
      (make-hash (for/list ((k (hash-keys h)))
                   (cons k (hash-ref h k))))))

;;----------------------------------------------------------------------

(define (mutable-hash . args)
  (hash->mutable (apply hash args)))

;;----------------------------------------------------------------------

(define/contract (mutable-hash? data)
  (-> hash? boolean?)
  (not (immutable? data)))

;;----------------------------------------------------------------------

(define/contract (hash-meld . hshs)
  (->* () () #:rest (non-empty-listof hash?) hash?)
  (cond [(= (length hshs) 1) (first hshs)]
        [else
         (define first-hsh (first hshs))
         (define is-immut? (immutable? first-hsh))
         ((if is-immut? identity hash->mutable)
          (apply hash-union
                 (map hash->immutable hshs)
                 #:combine (lambda (x y) y)))]))

;;----------------------------------------------------------------------

(define/contract (hash-slice the-hash keys)
  (-> hash? list? list?)
  (define default (gensym)) ; guaranteed unique value
  (filter-not (curry equal? default)
              (for/list ((k keys))
                (hash-ref the-hash k default))))

;;----------------------------------------------------------------------

(define/contract (not-equal? x y)
  (-> any/c any/c boolean?)
  (not (equal? x y)))

;;----------------------------------------------------------------------

(define/contract (not-null? lst)
  (-> list? boolean?)
  (not (null? lst)))

;;----------------------------------------------------------------------

(define/contract (one? arg)
  (-> number? (or/c #f number?))
  (if (equal? 1 arg) arg #f))

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

  (cond [(and (equal? p "") (not reject-empty-string))
         ""]
        [else
         (define appender  (if is-dir? path->directory-path identity))
         (define converter (if (string? p) string->path identity))
         (path->string (appender (converter p)))]))

;;----------------------------------------------------------------------

(define/contract (path-string->path   p #:dir? [is-dir? #f])
  (->* (path-string?) (#:dir? boolean?) path?)

  (define appender  (if is-dir? path->directory-path identity))
  (define converter (if (string? p) string->path identity))
  (appender (converter p)))

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

(define/contract (safe-file-exists? fpath)
  (-> any/c boolean?)

  (with-handlers ([exn:fail:contract? (lambda (e) #f)])
    (file-exists? fpath)))

;;----------------------------------------------------------------------

;; (define/contract (safe-hash-remove h #:key-is-list [key-is-list? #f] . keys)
;;    (->* (hash?) (#:key-is-list boolean?) #:rest (listof any/c) hash?)
;;
;; Mutable hashes use hash-remove! which returns (void).  Immutable
;; hashes use hash-remove and return the hash. Both will throw if you
;; use the wrong 'remove' function.  Both functions only remove a
;; single key at a time.  If you'd like to not deal with any of this,
;; use safe-hash-remove: it works on both mutable and immutable
;; hashes, it always returns the hash, and it removes as many keys as
;; you like, all in one go.
;;
;; Examples:
;;
;;    ; Here's a hash that includes a bunch of data that should be
;;    ; shown to the user and also a bunch of data needed by the
;;    ; application for other purposes:
;;    (define application-h (hash 'food? #t 'type 'fruit 'id 7 'added-to-db-at 1516997724))
;;
;;    ; Let's get it ready for output:
;;    (define output-h (safe-hash-remove h 'id 'added-to-db-at)) => only has 'food? and 'type keys
;;
;;    ; Same as above, but the keys are passed as a list -- perhaps
;;    ; they were generated by a DB query, or a map and it's a bother to
;;    ; unwrap them
;;    (define output-h (safe-hash-remove h '(id added-to-db-at))) => only has 'food? and 'type keys
;;
;;    ; Edge case: There is a key in your hash that really is a list.
;;    (define weird-h   (hash '(foo bar) 'x 'a 7 'b 8))
;;    (define wrong-way (safe-hash-remove '(foo bar)))     => WRONG!  hash unchanged
;;    (define wrong-way (safe-hash-remove '(foo bar) 'a))  => WRONG!  hash unchanged
;;    (define right-way (safe-hash-remove '(foo bar) #:key-is-list #t)) => (hash 'a 7 'b 8)
;;    (define right-way (safe-hash-remove '((foo bar))))                => (hash 'a 7 'b 8)
;;    (define also-right (safe-hash-remove '((foo bar) a)))             => (hash 'b 8)
;;
(define/contract (safe-hash-remove h #:key-is-list [key-is-list? #f] . keys)
  (->* (hash?) (#:key-is-list boolean?) #:rest (listof any/c) hash?)
  (define is-imm (immutable? h))
  (define keys-list
    (cond [key-is-list? keys] ; very unlikely, but included for completeness
          [(null? keys) keys]
          [(> (length keys) 1) keys]
          [(list? (car keys)) (car keys)]
          [else keys]))

  (for/fold ((hsh h))
            ((k keys-list))
    (if is-imm
        (hash-remove hsh k)
        (begin (hash-remove! hsh k) h))))

;;----------------------------------------------------------------------

(define/contract (safe-hash-set h  . args)
  (->* (hash?)
       ()
       #:rest (and/c list?
                     (lambda (lst)
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

(define/contract (safe-substring str start-idx [end #f] #:add-dots? [add-dots? #f])
  (->* (string? exact-nonnegative-integer?)
       (exact-nonnegative-integer? #:add-dots? boolean?)
       string?)
  (define len  (string-length str))
  (define end-idx (cond [(false? end)   len]
                        [(> end len)    len]
                        [else           end]))
  (when (< end-idx start-idx)
    (raise-arguments-error 'safe-substring "end must be >= start"
                           "start" start-idx
                           "end" end-idx))
  (define substr (substring str start-idx end-idx))
  (define prefix (if (zero? start-idx) "" "..."))
  (define suffix (if (<  end-idx len) "..." ""))

  (if add-dots?
      (~a prefix substr suffix)
      substr))

;;----------------------------------------------------------------------

(define/contract (sorted-hash-keys hsh [func symbol<?])
  (->* (hash?) ((unconstrained-domain-> boolean?)) list?)
  (sort (hash-keys hsh) func))

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

(define (symbol-string? x) ((or/c symbol? string?) x))

(define/contract (symbol-string->string x)
  (-> symbol-string? string?)
  (if (string? x) x (symbol->string x)))

(define/contract (symbol-string->symbol x)
  (-> symbol-string? symbol?)
  (if (string? x) (string->symbol x) x))

;;----------------------------------------------------------------------

(define/contract (thunk? item)
  (-> any/c boolean?)
  (and (procedure? item) (= 0 (procedure-arity item))))

;;----------------------------------------------------------------------

;;    Useful for coercing values to boolean for, e.g., inserting into DB
(define (true? x) (not (false? x)))

;;----------------------------------------------------------------------

(define/contract (delete-file-if-exists the-path [fail-return #f])
  (->* (path-string?) (any/c) any/c)
  (with-handlers ((exn:fail:filesystem? (lambda (e)
                                          (define msg (exn-message e))
                                          (cond [(regexp-match #px"No such file" msg)
                                                 fail-return]
                                                [else (raise e)])))

                  (match-anything raise))
    (delete-file the-path)
    1)) ; returns 1 instead of #f to make it easy to count files deleted

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

(define/contract (safe-build-path #:as-str? [as-str? #f] ; these two are aliases
                                  #:as-str [as-str #f] . args)
  (->* ()
       (#:as-str boolean? #:as-str? boolean?)
       #:rest (listof (or/c #f 'relative "" path-string?))
       path-string?)

  ;;    NB: Originally there was the '#:as-str' argument, before I was
  ;;    familiar with Racket's naming conventions.  Once I was, I
  ;;    started messing up the argument name all the time, so I added
  ;;    the '#:as-str?' in order to maintain backwards compatibility
  ;;    and still fit with the conventions.  Setting either one to #t
  ;;    means "yes, make this a string"

  ((if (or as-str as-str?) path->string identity)
   (apply build-path (filter (negate (or/c "" #f 'relative)) args))))

;;----------------------------------------------------------------------

(define/contract (unwrap-val val)
  (-> any/c any)
  (cond [(procedure? val) (val)]
        [(promise? val)   (force val)]
        [else             val]))

;;----------------------------------------------------------------------

;; (define/contract (with-temp-file proc #:path [the-path (make-temporary-file)])
;;
;;  Create a temporary file, pass its path to the specified proc, then
;;  delete it when the proc returns, even if an exception is thrown.
;;
;;  If you don't specify the path then one will be created for you.
(define/contract (with-temp-file proc #:path [the-path (make-temporary-file)])
  (->* ((-> path-string? any))
       (#:path path-string?)
       any)

  (dynamic-wind
    (thunk
     (define-values (dir filename ignore) (split-path the-path))
     (make-directory* dir)

     ; Note the race condition here.  Nothing to do about it.
     (when (not (file-exists? the-path))
       (define new-file (make-temporary-file))
       (rename-file-or-directory new-file the-path)))
    (thunk (proc the-path))
    (thunk
     (with-handlers ([exn:fail:filesystem?
                      (lambda (e)
                        (match (exn-message e)
                          [(pregexp #px"No such file or directory") #t]
                          [_ (raise e)]))])
       (delete-file the-path)))))

;;----------------------------------------------------------------------

(define/contract (hash-rename-key h old-key new-key)
  (-> hash? any/c any/c hash?)

  (when (not (hash-has-key? h old-key))
    (raise-arguments-error 'hash-rename-key
                           "no such key"
                           "old-key" old-key
                           "new-key" new-key
                           "hash" h))

  (when (hash-has-key? h new-key)
    (raise-arguments-error 'hash-rename-key
                           "destination key exists"
                           "old-key" old-key
                           "new-key" new-key
                           "hash" h))

  (safe-hash-remove
   (safe-hash-set h new-key (hash-ref h old-key))
   old-key))

;;----------------------------------------------------------------------

;; (define/contract (hash-remap h
;;                              #:remove    [remove-keys '()]
;;                              #:overwrite [overwrite   #f ]
;;                              #:add       [add         #f ]
;;                              #:rename    [remap       #f ]
;;                              )
;;   (->* (hash?) (#:rename hash? #:add hash? #:overwrite hash? #:remove list?) hash?)
;;
;;  Key mnemonic:  ROARen. Remove. Overwrite. Add. Rename.
;;
;;    This will munge hashes any way you like.  You can rename keys,
;;    remove keys, overwrite the value of keys, and add new keys.  The
;;    order of application is: remove -> overwrite -> add -> rename
;;
;;    The return value generally won't be eq? to the input, but it is
;;    guaranteed to be of the same type (mutable / immutable)
;;
;;    FIRST: remove any values we were told to remove via the #:remove list
;;        (hash-remap h #:remove '(key1 key2))
;;
;;    SECOND: overwrite any values from the original hash that we were
;;    told to overwrite via the #:overwrite hash.  If the new value is
;;    a procedure then it will be invoked and its result will be the
;;    new value.  The procedure must have the signature:
;;
;;        (-> hash? any/c any/c any/c)  ; takes a hash, key, orig-val.  Returns one value
;;
;;    The arguments will be: the hash we're updating, the key
;;    we're updating, and the original value.  It must return a
;;    single value.
;;
;;    If you actually want to pass in a procedure (e.g. if you're
;;    building a jumptable) then you'll have to wrap it like so:
;;
;;        (lambda (hsh key val orig-val)  ; the 'generate a value' procedure
;;            (lambda ...))               ; the procedure it generates
;;
;;    THIRD: add any additional keys that we were told to add.
;;    NOTE: This will throw an exception if you try to add a key
;;    that is already there.
;;
;;    FOURTH: rename keys
;;
;; (define h (hash 'group 'fruit   'color 'red    'type 'apple))
;;
;; (hash-remap h #:add (hash 'subtype 'honeycrisp))
;;    => (hash 'group 'fruit 'color 'red 'type 'apple 'subtype 'honeycrisp))
;;
;;    ; It's not legal to add a key that is already there.  If you want to do that, use #:overwrite
;; (hash-remap h #:add (hash 'group 'tasty))
;;    => EXCEPTION
;;
;; (hash-remap h #:remove '(group color)
;;    => (hash 'type 'apple)
;;
;; (hash-remap h #:rename (hash 'color 'shade  'type 'species )
;;    => (hash 'group 'fruit    'shade 'red    'species 'apple)
;;
;; (hash-remap h #:overwrite (hash 'group 'tasty   'color 'green   'type 'granny-smith))
;;    => (hash 'group 'tasty    'color 'green    'type 'granny-smith)
;;
;;    ; Alternatively, have the new value generated
;; (hash-remap (hash 'x 7 'y 9) #:overwrite (hash 'x (lambda (h k v) (add1 v))))
;;    =>       (hash 'x 8 'y 9)
;;
;; (hash-remap h  #:add       (hash 'vendor 'bob)
;;                #:overwrite (hash 'color 'green   'type 'granny-smith)
;;                #:remove    '(group)
;;                #:rename    (hash 'vendor 'seller))
;;    => (hash 'color 'green    'type 'granny-smith    'seller 'bob))
(define/contract (hash-remap h
                             #:rename    [remap #f]
                             #:remove    [remove-keys '()]
                             #:overwrite [overwrite #f]
                             #:add       [add #f]
                             )
  (->* (hash?) (#:rename hash? #:add hash? #:overwrite hash? #:remove list?) hash?)

  (let/ec return
    ; Just return unless we are going to rename, remove, overwrite, or
    ; add someting.
    (when (not (ormap perl-true? (list remap remove-keys overwrite add)))
      (return h))

    ; Okay, we're going to make some sort of change
    (define h-is-immutable? (immutable? h))

    (define union-func (if h-is-immutable? hash-union hash-union!))

    (define (default-hash) (if h-is-immutable? (hash) (make-hash)))
    (define overwrite-hash (or overwrite (default-hash)))
    (define add-hash       (or add       (default-hash)))
    (define remap-hash     (or remap     (default-hash)))

    ;; (say "original hash: " h
    ;;      "\n\t immutable?     " (immutable? h)
    ;;      "\n\t overwrite:     " overwrite-hash
    ;;      "\n\t add:           " add-hash
    ;;      "\n\t remap-hash:    " remap-hash)

    ;;    First, remove any values we were told to remove,
    (define base-hash
      (apply (curry safe-hash-remove h) remove-keys))

    ;;(say "hash after remove: " base-hash)

    ;;    Now, overwrite any values from the original hash that we
    ;;    were told to overwrite.  If the new value is a procedure
    ;;    then it will be invoked and its result will be the new
    ;;    value.  The procedure must have the signature:
    ;;
    ;;        (-> hash? any/c any/c any/c)  ; hash, key, orig-val, return one value
    ;;
    ;;    The arguments will be: the hash we're updating, the key
    ;;    we're updating, and the original value.  It must return a
    ;;    single value.
    ;;
    ;;    If you actually want to pass in a procedure (e.g. if you're
    ;;    building a jumptable) then you'll have to wrap it like so:
    ;;
    ;;        (lambda (hsh key val orig-val)  ; the 'generate a value' procedure
    ;;            (lambda ...))               ; the procedure it generates
    ;;
    ;;  NB: hash-union! modifies its target in place and then returns
    ;;  #<void>, because of course it does.  As a result, we need to
    ;;  check whether we're dealing with an immutable hash in order to
    ;;  know what to return.
    (define overwritten-hash
      (let ([hsh (union-func base-hash
                             overwrite-hash
                             #:combine/key (lambda (key orig-val overwrite-val)
                                             ;(say "entering combiner with args: " (string-join (map ~v (list key orig-val overwrite-val)) "; "))
                                             (cond [(procedure? overwrite-val)
                                                    ;(say "proc: " overwrite-val)
                                                    (overwrite-val base-hash key orig-val)]
                                                   [else overwrite-val])))])
        ;(say "finished overwrite")
        (if (void?  hsh) base-hash hsh))) ; void if we're dealing with mutable hash

    ;(say "hash with overwrites: " overwritten-hash)

    ;;    Next, add any additional keys that we were told to add.
    ;;
    ;;    NOTE: This will throw an exception if you try to add a key
    ;;    that is already there.
    (define hash-with-adds
      (let ([hsh (union-func overwritten-hash
                             add-hash
                             #:combine/key (lambda _ (raise-arguments-error
                                                      'hash-remap
                                                      "add-hash cannot include keys that are in base-hash"
                                                      "add-hash" add-hash
                                                      "hash to add (remove and overwrite already done)" overwritten-hash)))])
        (if (void? hsh) overwritten-hash hsh))) ; it's void when using mutable hash

    ;(say "hash-with-adds is: " hash-with-adds)
    ;(say "about to rename")
    ;;    Finally, rename keys
    (for/fold ([h hash-with-adds])
              ([(key val) remap-hash])
      ;(say "renaming in hash with key/val: " h "," key "," val)
      (hash-rename-key h key val))))

;;----------------------------------------------------------------------

(define/contract (ensure-field-set thing getter setter make-val [val-for-unset #f])
  (->* (any/c
        (-> any/c any/c)         ; getter
        (-> any/c any/c any/c)   ; setter
        thunk?)
       ;
       (any/c) ; the value that means this field was not set, typically #f
       ;
       any/c)

  (cond [(not-equal? val-for-unset (getter thing)) thing] ; field was set
        [else (setter thing (make-val))]))


(provide (all-defined-out))
