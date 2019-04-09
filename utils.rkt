#lang at-exp racket/base

(require (for-syntax racket/base)
         racket/bool
         racket/contract/base
         racket/contract/region
         racket/file
         racket/format
         racket/function
         racket/match
         racket/path
         racket/port
         racket/promise
         "hash.rkt")

(provide (all-from-out "hash.rkt")
         prefix-for-say
         __LINE__
         __FILE__         __FILE:__
         __WHERE__        __WHERE:__

         ->string
         
         !=
         both-or-neither
         
         12hr->24hr

         always-return
         always-true
         match-anything

         append-file
         safe-file-exists?
         delete-file-if-exists
         dir-and-filename
         ensure-directory-exists
         running-file-path
         running-file-dir
         safe-build-path
         with-temp-file

         not-equal?
         not-null?
         one?
         pad-digits
         path-string->string
         path-string->path
         perl-true?
         perl-false?
         px
         rand-val
         say
         silence
         symbol->keyword
         symbol-string?
         symbol-string->string
         symbol-string->symbol
         thunk?
         true?
         directory-empty?

         make-unique-string
         safe-substring
         empty-string?

         unwrap-val
         ensure-field-set
         )

;; Parameters
;; *) prefix-for-say : A string that will be prepended to all 'say' calls. Default: ""
;;
;; List of functions:
;; *) __FILE__, __LINE__, __WHERE__: current filepath/line/string of both
;; *) __FILE:__, __WHERE:__: same as previous but with ": " appended
;; *) ->string   : general purpose "convert stuff to string" that's smarter than ~a
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
;; *) safe-substring : like substring but won't puke if you ask for more than is available
;; *) say : macro that uses 'displayln' to output all
;;     args. e.g.: (say "num cows: " 7 ", and geese: " 8)
;; *) silence : eliminates all data sent to current-output-port (print, display, etc)
;; *) symbol->keyword
;; *) symbol-string?  : is it either a symbol or a string?
;; *) symbol-string->string and symbol-string->symbol
;; *) thunk?  ; is it a procedure of no arguments?
;; *) true? : opposite of false? (useful for coercing to boolean)
;; *) make-unique-string : returns a unique string derived from (gensym)
;; *) unwrap-val : call a thunk, force a promise, or return a val
;; *) with-temp-file : creates a temp file, ensures it will be deleted
;; WITH-TEMP-FILE IS NOT REENTRANT. ONCE YOU LEAVE THE FUNCTION,
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
  (not (apply = args)))

;;----------------------------------------------------------------------

(define both-or-neither (negate xor))

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

  (when (not (file-exists? source))
    (raise-arguments-error 'append-file
                           "source file must exist and does not"
                           "source" source
                           "dest" dest))

  ;;    Append file, return number of bytes in file afterwards so that
  ;;    we could verify the append if so desired.
  (with-output-to-file dest   #:mode 'binary #:exists 'append
    (thunk
     (with-input-from-file source #:mode 'binary
       (thunk
        (copy-port (current-input-port) (current-output-port))))))

  (file-size dest))

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

; dir-and-filename
;
; A wrapper around split-path.  split-path return three elements,
; where dir and filename are always paths.  dir-and-filename returns
; only two (the directory and the filename) and gives you options on
; how to present them.
;
; #:as-str? If #f (the default) then dir and filename are returned as
; paths. If #t, they are strings. (Note: #:relaxed? #t means one
; element might be "" even if the other is a path)
;
; #:relaxed? If #f (the default) then passing a single-element
; relative path (e.g. "foo") or the root directory (e.g. "/") raises
; an exception.  If #t then:
;    "foo" => (values "" "foo")  ; the non-empty-string item will be a
;    "/"   => (values "/" "")    ; path or string as per #:as-str?
;
;   IMPORTANT: EMPTY STRING IS NOT A VALID PATH.  If you attempt to
;   use it with functions that manipulate path-string? items then you
;   will get an exception.  #:relaxed? is useful if, e.g, you are
;   going to be storing the directory in a database as a string.  Use
;   safe-build-path to combine them without worrying about this.
;
(define/contract (dir-and-filename fp #:as-str? [as-str? #f] #:relaxed? [relaxed? #f]
                                   #:is-dir? [force-dir? #f])
  (->* ((or/c "" #f 'up 'same path-string?))
       (#:as-str? boolean?
        #:is-dir? boolean?
        #:relaxed? boolean?)
       (values (or/c "" 'up 'same path-string?) (or/c "" path-string?)))

  ;; Unless we are running in relaxed mode, you need to provide a
  ;; valid path-string value
  (match (list relaxed? fp)
    [(list #f (or "" #f 'up 'same))
     (raise-argument-error 'dir-and-filename "path-string?" fp)]
    ;
    [(list #t (or "" #f)) (values "" "")]
    [(list #t (or 'up 'same)) (values ""
                                      ((if as-str? path->string identity)
                                       (path->directory-path (build-path fp))))]
    [else
     (define-values (d f is-dir?) (split-path fp))

     ;(define as-str? #t) (define is-dir? #t) (define force-dir? #f)
     (define convert (compose (if as-str?  path-string->string identity)
                              (if (or force-dir? is-dir?) path->directory-path identity)
                              build-path)) ; this will handle 'up and 'same

     (match d
       ['relative (cond [relaxed? (values "" (convert f))]
                        [else
                         (raise-arguments-error  'dir-and-filename
                                                 "Cannot accept single-element relative paths unless you set #:relaxed? #t"
                                                 "path" fp)])]
       [#f        (cond [relaxed? (values "" (convert "/"))]
                        [else
                         (raise-arguments-error  'dir-and-filename
                                                 "Cannot accept root path (/) unless you set #:relaxed? #t"
                                                 "path" fp)])]
       [else
        (values (convert d) (convert f))])]))

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
  (equal? x ""))

;;----------------------------------------------------------------------

;  Check that a directory exists and create it if it doesn't.  Will
;  create intervening subdirectories as needed.  Uses make-directory*
;  but does more.
;
; When you call this, you're often going to have a path to a file
; that you're about to create and you want to make sure the
; directory exists.  You probably got that directory from
; split-path, or something like it.  This causes a few issues:
;
;    (split-path "/")  ; directory will be #f
;    (split-path ".")  ; directory will be 'relative, filename is 'same
;    (split-path "..") ; directory will be 'relative, filename is 'up
;
; In the first case, we can keep from blowing up by restoring the #f
; to "/", although I'm unsure what this will do on Windows.
; (Granted, I also don't care super much, since most Racket
; development is probably taking place on a *nix environment, if
; you're working on Windows you've got bigger issues, and doing it
; this way lets me make things easier for at least some users.)
;
; Other issue: 'up and 'same are not valid path-string? values, so
; we'll want to restore those as well.
;
; Finally, we can't handle the case where we get 'relative since we
; don't know what the 'filename' portion was.
;
(define/contract (ensure-directory-exists d)
  (-> (or/c #f 'up 'same 'relative path-string?) boolean?)

  (when (equal? d 'relative)
    (raise-arguments-error 'ensure-directory-exists
                           "'relative is not a valid path and cannot be converted to one.  If you used (split-path) to extract your directory value, consider (path-only) instead"
                           "directory" d))

  (define dir
    (path-only
     (path->directory-path
      (build-path (if (false? d) ;
                      "/"
                      d)))))

  (or (and (make-directory* dir) #f) ; make-directory* returns void, which is true, but we need to continue
      (directory-exists? dir)
      (raise (exn:fail:filesystem (~a "directory does not exist and could not be created: "
                                      dir)
                                  (current-continuation-marks)))))

;;----------------------------------------------------------------------

(define (running-file-path)
  (path->complete-path (find-system-path 'run-file)))

;;----------------------------------------------------------------------

(define (running-file-dir)
  (path-only (path->complete-path (find-system-path 'run-file))))

;;----------------------------------------------------------------------

;  safe-build-path
;
; A wrapper around build-path.  This accepts the optional arguments
; #:as-str? and #:as-str.  If either is set to #t (they default to #f)
; then the final result will be a string, otherwise it's a path.  This
; will filter out "", 'relative, and #f, so it's safe to feed in
; whatever came back from dir-and-filename.
;
;   Examples (assumes Unix system conventions, but Windows will work as normal)
;      (build-path "" "foo")                            ; EXCEPTION: "" is not a valid path
;      (safe-build-path "" "foo")                       ; #<path:foo>
;      (safe-build-path 'relative "foo" #:as-str? #t)   ; "foo"
;      (safe-build-path 'up   "foo" #:as-str? #t)       ; "../foo"
;      (safe-build-path 'same "foo" #:as-str? #t)       ; "./foo"
;      (safe-build-path 'same "foo" "bar")              ; "./foo/bar"
;
; NB: Originally there was the '#:as-str' argument, before I was
; familiar with Racket's naming conventions.  Once I was, I
; started messing up the argument name all the time, so I added
; the '#:as-str?' in order to maintain backwards compatibility
; and still fit with the conventions.  Setting either one to #t
; means "yes, make this a string"
;
(define/contract (safe-build-path #:as-str? [as-str? #f] ; these two are aliases
                                  #:as-str  [as-str  #f]
                                  . args)

  (->* ()
       (#:as-str boolean? #:as-str? boolean?)
       #:rest (listof (or/c #f "" 'up 'same 'relative path-string?))
       path-string?)

  ((if (or as-str as-str?) path->string identity)
   (apply build-path (filter (negate (or/c "" #f 'relative)) args))))


;;----------------------------------------------------------------------

(define/contract (make-unique-string [base ""])
  (->* () (string?) string?)
  (~a base (gensym)))

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
     (ensure-directory-exists dir)

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

; Takes a value that is expected to be an aggregator (hash, vector,
; struct, object, etc), checks one field of that value, and sets the
; field if it is not set.
;
; thing:    The value to be checked.  e.g. (hash 'id 1 'first-name "bob")
; getter:   Function to use to retrieve the value. e.g. (curryr hash-ref 'last-name)
; make-val: Generator for the new value.
; [optional] val-for-unset. The value to use as 'this field is not set'. Default: #f
; [optional] #:uses-mutation?  Set this to true if the setter is a mutating function
;
; make-val can be:
;   a thunk                               (called with no params)
;   a procedure of at least one argument  (called with the 'thing')
;   a non-procedure value                 (used directly)
;
(define/contract (ensure-field-set thing getter setter make-val [val-for-unset #f]
                                   #:uses-mutation? [uses-mutation? #f])
  (->* (any/c
        (-> any/c any/c)         ; getter
        (-> any/c any/c any/c)   ; setter
        any/c)                   ; make-val
       ;
       (any/c ; the value that means this field was not set, typically #f
        #:uses-mutation? boolean?)
       ;
       any/c)

  (define make-val-type (cond [(procedure? make-val) (procedure-arity make-val)]
                              [else 'raw-value]))

  (cond [(not-equal? val-for-unset (getter thing)) thing] ; field was set
        [else
         (let* ([val (match make-val-type
                       ['raw-value make-val]     ; not procedure
                       [0 (make-val)]            ; thunk
                       [else (make-val thing)])] ; procedure of 1+ args
                [result (setter thing val)])
           (if uses-mutation?
               thing ; mutation funcs often return (void), so can't trust new-val
               result))]))

;;----------------------------------------------------------------------
