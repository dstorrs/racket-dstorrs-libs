#!/usr/bin/env racket

#lang at-exp racket

(require dstorrs/utils
         dstorrs/test-more
         racket/runtime-path
         )

(define-runtime-path thisdir ".")

(ok #t "testing harness works")

(test-suite
 "perl-false?"
 (for ((v (list 0 0.0 -0 -0.0 "" #f (void) null)))
   (ok (perl-false? v) @~a{@v is false}))
 (not-ok (perl-false? "0foo")
         "'0foo' is not perl-false? but also not an exception" )
 )

(test-suite
 "append-file"

 ;;    Create two test files
 (define source (make-temporary-file))
 (define dest   (make-temporary-file))

 (with-output-to-file
   source
   #:exists 'replace
   (thunk
    (display "67890")))

 (with-output-to-file
   dest
   #:exists 'append
   (thunk
    (display "12345")))

 (is (append-file source dest)
     10
     "got correct number of bytes after append")

 (is (with-input-from-file
       dest
       (thunk
        (port->string)))
     "1234567890"
     "got correct contents")

 (delete-file source)
 (delete-file dest)
 )

(test-suite
 "->string"

 (is (->string 'foo) "foo" "worked for symbol foo")
 (is (->string "foo") "foo" "worked for string foo")
 (is (->string 8) "8" "worked for int 8")
 (is (->string 8.0) "8.0" "worked for num 8.0")
 (is (->string +inf.0) "+inf.0" "worked for num +inf.0")
 (is (->string -inf.0) "-inf.0" "worked for num -inf.0")
 (is (->string +nan.0) "+nan.0" "worked for num +nan.0")
 (is (->string -nan.0) "+nan.0" "worked for num -nan.0") ; weird conversion but correct
 (is (->string (list #\A)) "A" "worked for (list char)")
 (is (->string (list "x" "y" "z")) "xyz" "worked for (list string)")
 (is (->string (vector "x" "y" "z")) "xyz" "worked for (vector string, string, string)")
 (is (->string (vector 8 "y" "z")) "8yz" "worked for (vector num, string, string)")

 (is (->string #f)
     "#f"
     "(->string #f) works")

 (struct test-struct (x))
 (is (->string (test-struct 'x))
     "#<test-struct>"
     "(->string struct) works")

 (lives (thunk (->string (exn:fail "foo" (current-continuation-marks))))
        "(->string exn:fail) lives")
 (lives (thunk (->string (list 8 8 #f)))
        "(->string (list 8 8 #f)) throws")
 (lives (thunk (->string (vector 8 8 #f)))
        "(->string (list 8 8 #f)) throws")
 )

(test-suite
 "rand-val"
 (for ((v (list 1000 "foo" 'bar #\A (list 1 2 3) (vector 1 2 3))))
   (let ((r (rand-val v)))
     (is-type r string? (~a "got expected type for " r))
     (ok (regexp-match (pregexp (~a (->string v) "-\\d+")) r)
         (~a "rand-value is as expected for " r))
     )
   )
 (is-type (rand-val) string? "generic (rand-val) yields string")
 (ok (regexp-match (pregexp "\\d+") (rand-val))
     (~a "rand-value is as expected for (rand-val)"))
 (is-type (rand-val #:post string->number) number? "got expected type for string->number")
 (let ((lst (rand-val #:post string->list)))
   (is-type lst list? "got list from #:post string->list")
   (ok (andmap char? lst)
       "when post processing with string->list, we get a list of characters, as expected"))

 );;test-suite

(when #t
  (test-suite
   "with-temp-file"

   (define the-path "")

   (is (with-temp-file
         (lambda (filepath)
           (set! the-path filepath)
           (ok (file-exists? filepath)
               "the temporary file was created")
           (is filepath the-path "the-path was properly updated")
           7))
       7
       "with-temp-file returns its final value")
   (ok (not (file-exists? the-path)) "the file was deleted after with-temp-file returned")

   (lives (thunk
           (with-temp-file
             (lambda (filepath)
               (delete-file filepath)
               (ok (not (file-exists? filepath)) "inside proc, successfully deleted file"))))
          "successfully completed with-temp-file where I deleted the file inside the proc and it didn't explode at the fact that the file wasn't there when it exited")

   (struct fake-exn ())
   (throws (thunk
            (with-handlers ((string? (lambda (e)
                                       (ok (not (file-exists? the-path)) "the file was still deleted after with-temp-file threw an exception")
                                       (raise (fake-exn))
                                       ))
                            (true? (lambda (e)
                                     ;(say "inside catch-all handler. e was: " e)
                                     (ok #f (~a "expected to throw the string 'foo', actually threw: " e)))))
              (with-temp-file
                (lambda (filepath)
                  (set! the-path filepath)
                  (raise "foo")))))
           fake-exn?
           "with-temp-file died as expected and was processed by the with-handlers as expected")

   (let ([test-path (make-temporary-file)])
     (ok (file-exists? test-path) "before with-temp-file, temporary file was successfully created")
     (ok (not (file-exists? the-path)) "before with-temp-file, the-path does not exist")

     (with-temp-file
       #:path test-path
       (lambda (filepath)
         (set! the-path filepath)
         (ok (file-exists? test-path) "test path was passed in and it exists")
         (is filepath test-path "file is at the specified path")
         (is filepath the-path "the-path was properly updated")))
     (ok (not (file-exists? the-path)) "after with-temp-file, the-path does not exist"))

   (with-temp-file
     #:path "/tmp/askjghwkhaedjhfodjoewjowehgaegr/foo"
     (lambda (filepath)
       (set! the-path filepath)
       (ok (file-exists? filepath) "test path was passed in and it exists")
       (is filepath the-path "the-file was updated")))
   (ok (not (file-exists? the-path)) "after with-temp-file, the-path does not exist")

   )
  )

(when #t
  (test-suite
   "safe-file-exists?"

   (lives (thunk
           (with-temp-file
             (lambda (fpath)
               (or
                (ok (and (file-exists? fpath) (safe-file-exists? fpath))
                    "temp file exists")
                (raise "could not find temp file"))

               (safe-file-exists? #f)
               (safe-file-exists? ""))))
          "safe-file-exists? doesn't throw on bad input")

   (is-false (safe-file-exists? "") "safe-file-exists? returns #f on empty string")
   (is-false (safe-file-exists? #f) "safe-file-exists? returns #f on #f")
   ))

(test-suite
 "safe-hash-remove"

 (define hash-imm (hash 'a 1 'b 2 'c 3))
 (define (hash-mut) (make-hash '((a . 1) (b . 2) (c . 3))))

 (is (safe-hash-remove hash-imm 'a)
     (hash 'b 2 'c 3)
     "(safe-hash-remove hash-imm 'a) worked")

 (is (safe-hash-remove hash-imm 'x)
     (hash 'a 1 'b 2 'c 3)
     "(safe-hash-remove hash-imm 'x) worked")

 (is (safe-hash-remove hash-imm 'a 'x)
     (hash 'b 2 'c 3)
     "(safe-hash-remove hash-imm 'a 'x) worked")

 (is (safe-hash-remove hash-imm '(a x))
     (hash 'b 2 'c 3)
     "(safe-hash-remove hash-imm '(a x)) worked (list of keys)")

 (is (safe-hash-remove (hash '(a x) 7 'b 3) '(a x) #:key-is-list #t)
     (hash 'b 3)
     "(safe-hash-remove (hash '(a x) 7 b 3) '(a x) #:key-is-list #t) worked")

 (is (safe-hash-remove (hash-mut) 'a)
     (make-hash '((b . 2) (c . 3)))
     "(safe-hash-remove hash-mut 'a) worked")

 (is (safe-hash-remove (hash-mut) 'x)
     (make-hash '((a . 1) (b . 2) (c . 3)))
     "(safe-hash-remove hash-mut 'x) worked")

 (define h (hash-mut))
 (let ((res (safe-hash-remove h 'a 'c 'x)))
   (ok (eq? h res) "safe-hash-remove returns the same hash when given a mutable hash")
   (is res
       (make-hash '((b . 2)))
       "(safe-hash-remove hash-mut 'a 'c 'x) worked"))
 );; test-suite

(test-suite
 "hash->mutable and hash->immutable"

 (define mut (make-hash '((a . 1))))
 (ok (not (immutable? mut)) "mut is mutable")
 (ok (immutable? (hash->immutable mut)) "hash->immutable mut is immutable")

 (define immut (hash 'a 1))
 (ok (immutable? immut) "immut is immutable")
 (ok (not (immutable? (hash->mutable immut))) "hash->mutable immut is mutable")
 )

(test-suite
 "hash-meld"

 (define x (hash 'a 1 'b 2))
 (define y (hash 'b 3 'c 4))
 (define z (hash 'c 5 'd 6))
 (define a (make-hash '((d . 7) (e . 8))))
 (is (hash-meld x y) (hash 'a 1 'b 3 'c 4) "hash-meld works with two immutable hashes")
 (is (hash-meld x y z a) (hash 'a 1 'b 3 'c 5 'd 7 'e 8) "hash-meld works with three immutable and + one mutable hashes (result is immutable since first hash was immutable)")
 (is (hash-meld a x y z)
     (hash->mutable (hash 'a 1 'b 3 'c 5 'd 6 'e 8))
     "hash-meld works with three immutable and + one mutable hashes (result is immutable since first hash was immutable)")
 )

(test-suite
 "safe-hash-set"
 (define hash-imm (hash 'a 1 'b 2 'c 3))
 (ok (immutable? hash-imm) "using immutable hash for next test")
 (is (safe-hash-set hash-imm 'b 5)
     (hash 'a 1 'b 5 'c 3)
     "can handle an immutable hash when specifying one key and one value")

 (is (safe-hash-set hash-imm 'a 3 'b 9 'd 8)
     (hash 'a 3 'b 9 'c 3 'd 8)
     "can handle an immutable hash when specifying multiple arguments")

 (throws (thunk (safe-hash-set hash-imm 'x))
         #px"safe-hash-set: contract violation"
         "safe-hash-set throws if given an odd number of arguments")




 (let ((hash-mut (make-hash '((a . 1) (b . 2) (c . 3)))))
   (is (safe-hash-set  hash-mut 'b 5)
       (make-hash '((a . 1) (b . 5) (c . 3)))
       "can handle a mutable hash"))

 (let* ((hash-mut  (make-hash '((a . 1) (b . 2) (c . 3))))
        (h (safe-hash-set hash-mut 'a 3 'b 9 'd 8)))
   (ok (eq? h hash-mut) "the hash returned from safe-hash-set is the same one sent in if you sent a mutable hash")
   (is h
       (hash->mutable (hash 'a 3 'b 9 'c 3 'd 8))
       "can handle a mutable hash when specifying multiple arguments"))

 (dies (thunk (safe-hash-set #f 'a 7))
       "safe-hash-set requires a hash for the first argument")
 )

(test-suite
 "mutable-hash"

 (let ((h (mutable-hash 'a 1)))
   (ok (and (hash? h) (not (immutable? h)))
       "(mutable-hash 'a 1)) works"))

 (let ((h (mutable-hash 'a 1 'b 2)))
   (ok (and (hash? h) (not (immutable? h)))
       "(mutable-hash 'a 1 'b 2)) works"))
 )

(when #t
  (test-suite
   "hash-keys->strings and hash-keys->symbols"
   (is (hash-keys->strings (hash "foo" 7 'bar 8))
       (hash "foo" 7 "bar" 8)
       "(hash-keys->strings works")

   (is (hash-keys->symbols (hash "foo" 7 'bar 8))
       (hash 'foo 7 'bar 8)
       "(hash-keys->symbols works")

   (is (hash-keys->strings (hash "foo" 7 'bar 8 0 'c))
       (hash "foo" 7 "bar" 8 "0" 'c)
       "(hash-keys->strings works on things with keys that are not symbol/string")

   (is (hash-keys->strings #:dash->underscore? #t (hash "foo-bar" 1 'bar-z 8))
       (hash "foo_bar" 1 "bar_z" 8)
       "hash-keys->strings #:dash->underscore? #t converts, e.g., 'foo-bar to \"foo_bar\"")

   (is (hash-keys->symbols (hash "foo" 7 'bar 8 0 'c))
       (hash 'foo 7 'bar 8 '|0| 'c)
       "(hash-keys->symbols works on things with keys that are not symbol/string")

   (is-type (hash-keys->symbols (mutable-hash "foo" 7 'bar 8))
            (negate immutable?)
            "hash-keys->symbols preserved the (im)mutability of the hash")

   (is-type (hash-keys->strings (hash "foo" 7 'bar 8))
            immutable?
            "hash-keys->strings preserved the (im)mutability of the hash")
   )
  )

(when #t
  (test-suite
   "dir-and-filename"

   (let-values (((dir fname) (dir-and-filename "/foo/bar")))
     (is dir (string->path "/foo/") "got correct dir for /foo/bar")
     (is fname (string->path "bar") "got correct filename for /foo/bar"))

   (let-values (((dir fname) (dir-and-filename "foo/bar")))
     (is dir (string->path "foo/") "got correct dir for foo/bar")
     (is fname (string->path "bar") "got correct filename for foo/bar"))

   (let-values (((dir fname) (dir-and-filename "foo/bar/")))
     (is dir (string->path "foo/") "got correct dir for foo/bar")
     (is fname (string->path "bar/") "got correct filename for foo/bar/"))

   (let-values (((dir fname) (dir-and-filename "/foo")))
     (is dir (string->path "/") "got correct dir for /foo")
     (is fname (string->path "foo") "got correct filename for /foo"))

   (throws (thunk (dir-and-filename "foo"))
           #rx"Cannot accept single-element relative paths"
           "dir-and-filename throws if given a one-element relative path WITHOUT trailing slash ('foo')")

   (throws (thunk (dir-and-filename "foo/"))
           #rx"Cannot accept single-element relative paths"
           "dir-and-filename throws if given a one-element relative path WITH a trailing slash ('foo/')")

   (throws (thunk (dir-and-filename "/"))
           #rx"Cannot accept root path"
           "dir-and-filename throws if given the root path")

   (let-values ([(dir fname) (dir-and-filename "/foo/bar" #:as-str? #t)])
     (is dir "/foo/" "dir-and-filename accepts the #:as-str? arg and returns dir as string")
     (is fname "bar" "dir-and-filename accepts the #:as-str? arg and returns fname as string"))
   )
  )

(test-suite
 "path-string->(path | string)"

 (define vals (list "foo" "/bar" "/baz/foo"))
 (define dirs (map (curryr string-append "/") vals))

 ;;    If you pass in a string without setting #:dir? then you
 ;;    should get the same string back.  Checks strings both
 ;;    with and without a trailing slash.
 (for ((p (append vals dirs)))
   (is (path-string->string p)
       p
       @~a{path-string->string @p works}))

 ;;    If you pass in strings and set #:dir? then what you get
 ;;    back should have a trailing slash.
 (for ((p vals)
       (d dirs))
   (is (path-string->string p #:dir? #t)
       @~a{@|p|/}
       @~a{(path-string->string @p #:dir? #t) works})

   (is (path-string->string d #:dir? #t)
       d
       @~a{(path-string->string @d #:dir? #t) works}))

 (is (path-string->string "")
     ""
     "path-string->string handles the empty string when #:dir? not set")

 (is (path-string->string "" #:dir? #t)
     ""
     "path-string->string handles the empty string when #:dir? IS set")

 (is (path-string->string "" #:dir? #f)
     ""
     "path-string->string handles the empty string when #:dir? IS set and is #f")

 (throws (thunk (path-string->string "" #:reject-empty-string #t))
         exn:fail:contract?
         "path-string->string handles the empty string when #:dir? IS set and is #f")

 (throws (thunk (path-string->string "" #:reject-empty-string? #t))
         exn:fail:contract?
         @~a{path-string->string fails on empty string with #:reject-empty-string? set but no #:dir})

 (throws (thunk (path-string->string "" #:reject-empty-string? #t #:dir? #t))
         exn:fail:contract?
         @~a{path-string->string fails on empty string with #:reject-empty-string? #t and #:dir #t})

 (throws (thunk (path-string->string "" #:reject-empty-string? #t #:dir? #f))
         exn:fail:contract?
         @~a{path-string->string fails on empty string with #:reject-empty-string? #t and #:dir #f})


 ;;    If you pass in a path then you should get back the
 ;;    equivalent string as per path->string.  It doesn't
 ;;    matter if you set #:dir? or not.
 (for ((p (map string->path vals))
       (d (map string->path dirs)))
   (for ((x (list p d)))
     (is (path-string->string x)
         (path->string x)
         @~a{(path-string->string @x) works})

     (is (path-string->string x #:dir? #t)
         (path->string (path->directory-path x))
         @~a{(path-string->string @x #:dir? #t) works})))
 ); test-suite

(test-suite
 "__FILE__, __LINE__, __WHERE__, and their ...: versions"
 (is __LINE__
     (- (syntax-line #'here) 1)
     "__LINE__ works")

 (is __FILE__
     (syntax-source #'here)
     "__FILE__ works")

 (is __FILE:__
     (~a (syntax-source #'here) ": ")
     "__FILE:__ works")

 (is __WHERE__
     (let ((line (- (syntax-line #'here) 1))
           (fpath (syntax-source #'here)))
       (~a "file:" fpath " (line:" line ")"))
     "__WHERE__ works")

 (is __WHERE:__
     (let ((line (- (syntax-line #'here) 1))
           (fpath (syntax-source #'here)))
       (~a "file:" fpath " (line:" line "): "))
     "__WHERE__ works")

 );test-suite

(test-suite
 "say"

 (is (with-output-to-string
       (thunk (say "foo")))
     "foo\n"
     "basic 'say foo' works")

 (is (with-output-to-string
       (thunk (say "foo" "bar")))
     "foobar\n"
     "'say foo bar' works")

 (is (with-output-to-string
       (thunk (say)))
     "\n"
     "'(say)' works")

 (parameterize ((prefix-for-say "foo"))
   (is (with-output-to-string
         (thunk (say "bar")))
       "foobar\n"
       "prefix-for-say 'foo' is respected"))

 (parameterize ((prefix-for-say 87))
   (is (with-output-to-string
         (thunk (say "bar")))
       "87bar\n"
       "prefix-for-say with number is respected"))

 (parameterize ((prefix-for-say (hash)))
   (is (with-output-to-string
         (thunk (say "bar")))
       "#hash()bar\n"
       "prefix-for-say with number is respected"))

 (parameterize ((prefix-for-say (hash 'a 67 'b "foo")))
   (is (with-output-to-string
         (thunk (say "bar")))
       "#hash((a . 67) (b . foo))bar\n"
       "prefix-for-say with hash is respected"))
 )

(test-suite
 "directory-empty?"

 (call/cc
  (lambda (return)
    (define homedir (find-system-path 'home-dir))
    (define test-dir (build-path homedir (rand-val "jlfkgjlahkfjhkfjhkdshwouwou")))
    (when (directory-exists? test-dir)
      (return (say (string-append "Not doing tests for directory-empty? because testdir '" (path->string test-dir) "' exists"))))

    (dynamic-wind
      (thunk
       (make-directory test-dir)
       (ok (directory-exists? test-dir) "Successfully created test directory")
       (ok (directory-empty? test-dir) "Newly created directory is empty")
       )
      (thunk
       (with-output-to-file
         (build-path test-dir "test-file")
         (thunk
          (display "test value")))
       (not-ok (directory-empty? test-dir) "as expected, found that directory was NOT empty after creating test file"))
      (thunk (delete-directory/files test-dir)))

    (not-ok (directory-exists? test-dir) "Cleaned up properly; test directory was removed")
    ))
 )

(when #t
  (test-suite
   "silence"
   (is (with-output-to-string (thunk (display "foo")))
       "foo"
       "can catch output as expected")

   (is (with-output-to-string
         (thunk
          (silence (display "foo"))
          ))
       ""
       "(silence) eliminates output")

   (is (silence (display "foo") 7)
       7 "silence does not eat the normal return value")

   );test-suite
  );when

;;----------------------------------------------------------------------

(when #t
  (test-suite
   "ensure-directory-exists"

   (define dir (rand-val "/tmp/test-dir"))

   (is-false (directory-exists? dir) "at start of testing, directory does not exist")
   (ok (ensure-directory-exists dir) "ensure-directory-exists returns true when the directory didn't exist")
   (ok (directory-exists? dir) "after call to ensure, directory exists")
   (ok (ensure-directory-exists dir) "ensure-directory-exists returns true when the directory did exist")
   (file-or-directory-permissions dir 0)
   (throws (thunk (ensure-directory-exists (build-path dir "foo")))
           exn:fail:filesystem?
           "throws when could not create a directory due to permissions")
   (file-or-directory-permissions dir 511)
   (delete-directory dir)
   )
  )

;;----------------------------------------------------------------------

(when #t
  (test-suite
   "safe-build-path"

   (define bp build-path)
   (is (safe-build-path "/foo") (bp "/foo") "(sbp \"/foo\")")
   (is (safe-build-path "/foo" "bar") (bp "/foo" "bar") "(sbp \"/foo\" \"bar\")")
   (is (safe-build-path "/foo" "bar" #:as-str #t)
       (path->string (bp "/foo" "bar"))
       "(sbp \"/foo\" \"bar\" #:as-str #t)")
   (is (safe-build-path "/foo" "")
       (bp "/foo")
       "(sbp \"/foo\" \"\")")
   (is (safe-build-path "/foo" "" "")
       (bp "/foo")
       "(sbp \"/foo\" \"\" \"\")")
   (is (safe-build-path "" "/foo" "" "baz")
       (bp "/foo/baz")
       "(sbp \"\" \"/foo\" \"\" \"baz\")")
   (is (safe-build-path "" "/foo" "" "baz" #:as-str #t)
       (path->string (bp "/foo/baz"))
       "(sbp \"\" \"/foo\" \"\" \"baz\" #:as-str #t)")
   (is (safe-build-path #f "/foo" #f "baz" #:as-str #t)
       (path->string (bp "/foo/baz"))
       "(sbp #f \"/foo\" #f \"baz\" #:as-str #t)")
   (is (safe-build-path 'relative "foo")
       (bp "foo")
       "(safe-build-path 'relative \"foo\"")

   )
  )

;;----------------------------------------------------------------------

(when #t
  (test-suite
   "!="
   (ok (!= 7 8 9) "(!= 7 8 9) works")
   (is-false (!= 7 7 7) "(!= 7 7 7) works")
   )
  )

;;----------------------------------------------------------------------

(when #t
  (test-suite
   "thunk?"

   (ok (thunk? (thunk 7)) "thunk? correctly identified a thunk")
   (is-false (thunk? (lambda (x) 7)) "thunk? correctly identified a proc of one arg is not a thunk")
   (is-false (thunk? 7) "thunk? correctly identified that 7 is not a thunk")
   ))

;;----------------------------------------------------------------------

(when #t
  (test-suite
   "always-return"

   (is-type (always-return 7) procedure? "always-return returns a function")
   (is ((always-return 7) 'a 'b) 7 "((always-return 7) 7 8 accepts 2 args, returns 7")
   (is ((always-return 7) 'a 'b 'c 0 1) 7 "(always-return 7) accepts many args, returns 7")
   )
  )

;;----------------------------------------------------------------------

(when #t
  (test-suite
   "empty-string?"

   (is-false (empty-string? 7) "7 is not the empty string")
   (is-false (empty-string? 'x) "'x is not the empty string")
   (is-false (empty-string? '(7 a)) "'(7 a) is not the empty string")
   (is-false (empty-string? (hash 7 'a)) "(hash 7 'a) is not the empty string")
   (ok (empty-string? "") "(empty-string? \"\" is true")
   )
  )

;;----------------------------------------------------------------------

(when #t
  (test-suite
   "hash-rename-key"

   (define h (hash 'a 1 'b 2 'c 3))
   (is (hash-rename-key h 'a 'x)
       (hash 'x 1 'b 2 'c 3)
       "successfully renamed key 'a to 'x")

   (throws (thunk (hash-rename-key h 'zot 'zag))
           #px"no such key"
           "trying to rename a non-existent key throws")
   (throws (thunk (hash-rename-key h 'a 'b))
           #px"destination key exists"
           "throws when you try to rename to a key that already exists")
   ))

;;----------------------------------------------------------------------

(when #t
  (test-suite
   "hash-remap"

   (is (hash-remap (hash) #:rename (hash))
       (hash)
       "empty hash is unchanged")

   (define h (hash 'a 1 'b 2 'c 3 'd 4))
   (is (hash-remap h #:rename (hash 'a 'e))
       (hash 'e 1 'b 2 'c 3 'd 4)
       "remapping only one key of immutable hash works")

   (is (hash-remap h #:rename (hash 'a 'e 'b 'f))
       (hash 'e 1 'f 2 'c 3 'd 4)
       "remapping multiple keys of immutable hash works")

   (is (hash-remap h #:remove (list 'a))
       (hash 'b 2 'c 3 'd 4)
       "removing one key of immutable hash works")

   (is (hash-remap h #:remove (list 'a 'b))
       (hash 'c 3 'd 4)
       "removing multiple keys of immutable hash works")

   (is (hash-remap h
                   #:remove (list 'a)
                   #:rename  (hash 'b 'e))
       (hash 'e 2 'c 3 'd 4)
       "removing one key and remapping one key of immutable hash works")

   (is (hash-remap h
                   #:rename (hash 'a 'e 'b 'f)
                   #:remove '(c d))
       (hash 'e 1 'f 2)
       "removing multiple keys while remapping multiple keys of immutable hash works")


   (define (make-test-hash)
     (mutable-hash
      'a 1 'b 2 'c 3 'd 4))

   (is (hash-remap (make-test-hash) #:rename (hash 'a 'e))
       (mutable-hash 'e 1 'b 2 'c 3 'd 4)
       "remapping only one key of mutable hash works")

   (is (hash-remap (make-test-hash) #:rename (hash 'a 'e 'b 'f))
       (mutable-hash 'e 1 'f 2 'c 3 'd 4)
       "remapping multiple keys of mutable hash works")

   (is (hash-remap (make-test-hash) #:remove '(a))
       (mutable-hash 'b 2 'c 3 'd 4)
       "removing one key of mutable hash works")

   (is (hash-remap (make-test-hash) #:remove '(a b))
       (mutable-hash 'c 3 'd 4)
       "removing multiple keys of mutable hash works")

   (is (hash-remap (make-test-hash)
                   #:rename (hash 'b 'e)
                   #:remove '(a))
       (mutable-hash 'e 2 'c 3 'd 4)
       "removing one key and remapping one key of mutable hash works")

   (is (hash-remap (make-test-hash)
                   #:remove '(c d)
                   #:rename (hash 'a 'e 'b 'f))
       (mutable-hash 'e 1 'f 2)
       "removing multiple keys while remapping multiple keys of mutable hash works")

   (throws (thunk (hash-remap (make-test-hash) #:rename (hash 'a 'b)))
           #px"destination key exists"
           "can't rename a hash key to an existing hash key")

   (throws (thunk (hash-remap (make-test-hash) #:rename (hash 'x 'y)))
           #px"no such key"
           "can't rename a hash key that isn't there")

   (lives (thunk (hash-remap (make-test-hash) #:remove '(x)))
          "removing a hash key that isn't there is fine")

   (is (hash-remap (make-test-hash)
                   #:rename (hash 'a 'e 'b 'f)
                   #:remove '(c d)
                   #:add (hash 'x 7 'y 8))
       (mutable-hash 'e 1 'f 2 'x 7 'y 8)
       "can remove multiple keys, rename multiple keys, and add multiple keys all at once")

   (is (hash-remap (mutable-hash 'a 1 'b 2 'c 3 'd 4 'z 10)
                   #:rename (hash 'a 'e 'b 'f)
                   #:add (hash 'x 7 'y 8)
                   #:remove '(c d)
                   #:overwrite (hash 'a 17 'z 88))
       (mutable-hash 'e 17 'f 2 'z 88 'x 7 'y 8)
       "can (remove | rename | add | overwrite) multiple keys all at once")


   (is (hash-remap (hash 'a 1 'b 2)
                   #:overwrite (hash 'a (lambda (hsh key orig-val)
                                          (~a (add1 orig-val)))))
       (hash 'a "2" 'b 2)
       "can provide funcs as values with which to overwrite things")

   ))

;;----------------------------------------------------------------------

(when #t
  (test-suite
   "unwrap-val"

   (for ((i (list 7 "foo" 'x (vector 7) '(x y z))))
     (is (unwrap-val i)
         i
         (~a "unwrapping a general value (" i ") returns that value")))

   (is (unwrap-val (thunk 8))
       8
       "(unwrap-val (thunk 8)) returns 8")

   (is (unwrap-val (lazy 9))
       9
       "(unwrap-val (lazy 9)) returns 9")
   ))


;;----------------------------------------------------------------------

(when #t
  (test-suite
   "sorted-hash-keys"

   (is (sorted-hash-keys (hash 'foo 1 'bar 2 'baz 3))
       '(bar baz foo)
       "success: (sorted-hash-keys (hash 'foo 1 'bar 2 'baz 3))")

   (is (sorted-hash-keys (hash "foo" 1 "bar" 2 "baz" 3) string<?)
       '("bar" "baz" "foo")
       "success: (sorted-hash-keys (hash \"foo\" 1 \"bar\" 2 \"baz\" 3) string<?)")

   (is (sorted-hash-keys (hash 3 "foo" 2 "bar" 1 "baz") <)
       '(1 2 3)
       "success: (sorted-hash-keys (hash 3 \"foo\" 2 \"bar\" 1 \"baz\") <)")

   (is (sorted-hash-keys (hash 3 "foo" 2 "bar" 1 "baz") >)
       '(3 2 1)
       "success: (sorted-hash-keys (hash 3 \"foo\" 2 \"bar\" 1 \"baz\") >)")
   ))

;;----------------------------------------------------------------------

(when #t
  (test-suite
   "hash-slice"
   (define h (hash 'a 1 'b 2 'c 3 'd 4))

   (lives (thunk
           (is (hash-slice (hash) '(a b c))
               '()
               "success: (hash-slice (hash) '(a b c)) returns empty list")
           )
          "slicing an empty hash didn't die.")
   (is (hash-slice h '(a b c d))
       '(1 2 3 4)
       "success: (hash-slice h '(a b c d))")

   (is (hash-slice h '(a b))
       '(1 2)
       "success: (hash-slice h '(a b))")

   (is (hash-slice h '(d c a b))
       '(4 3 1 2)
       "success: (hash-slice h  '(d c a b))")
   ))

;;----------------------------------------------------------------------

(when #t
  (test-suite
   "delete-file-if-exists"

   (define test (make-temporary-file))
   (ok (file-exists? test) "before deletion, file exists")

   (is (delete-file-if-exists test)
       1
       "(delete-file-if-exists test) worked and returned 1 to say file existed")

   (is-false (file-exists? test) "after deletion, file is deleted")

   (lives (thunk
           (is-false (delete-file-if-exists test)
                     "(delete-file-if-exists test) on a deleted file returned #f to say file not deleted"))
          "(delete-file-if-exists test) on a deleted file lived")

   (lives (thunk
           (is (delete-file-if-exists test 0)
               0
               "(delete-file-if-exists test 0) on a deleted file returned 0 to say file not deleted"))
          "(delete-file-if-exists test 0) on a deleted file lived")
   ))

;;----------------------------------------------------------------------

(when #t
  (test-suite
   "safe-substring"

   (is (safe-substring "" 0 1)
       ""
       "(safe-substring \"\" 0 1) works")

   (is (safe-substring "foobar" 0 1)
       "f"
       "(safe-substring \"foobar\" 0 1) works")

   (is (safe-substring "foobar" 0 100)
       "foobar"
       "(safe-substring \"foobar\" 0 100) works")

   (is (safe-substring "foobar" 0 1 #:add-dots? #t)
       "f..."
       "can append dots at the end if you ask for them")

   (is (safe-substring "foobar" 1 100 #:add-dots? #t)
       "...oobar"
       "can append dots at the start if you ask for them")

   (is (safe-substring "foobar" 1 3 #:add-dots? #t)
       "...oo..."
       "can append dots on both sides if you ask for them")

   (is (safe-substring "foobar" 0 100 #:add-dots? #t)
       "foobar"
       "won't append dots if the whole string is returned, even if you ask")

   (throws (thunk (safe-substring "foobar" 100 0))
           #px"end must be >= start"
           "start must be <= end")
   ))

;;----------------------------------------------------------------------

(when #t
  (test-suite
   "symbol-string?, symbol-string->string, symbol-string->symbol"
   (ok (symbol-string? "foo") "(symbol-string? \"foo\") works")
   (ok (symbol-string? 'foo) "(symbol-string? 'foo) works")
   (is-false (symbol-string? 7) "(symbol-string? 7) works")

   (is (symbol-string->string 'foo)
       "foo"
       "(symbol-string->string 'foo) works")

   (is (symbol-string->string "foo")
       "foo"
       "(symbol-string->string \"foo\") works")

   (is (symbol-string->symbol "foo")
       'foo
       "(symbol-string->symbol \"foo\") works")

   (is (symbol-string->symbol 'foo)
       'foo
       "(symbol-string->symbol 'foo) works")
   ))

;;----------------------------------------------------------------------

(when #t
  (test-suite
   "ensure-field-set"

   (is (ensure-field-set (hash 'foo 11)
                         (curryr hash-ref 'foo)
                         (lambda (h val) (hash-set h 'foo val))
                         (thunk 9))
       (hash 'foo 11)
       "ensure-field-set returns its value undisturbed when the field is set")

   (is (ensure-field-set (hash)
                         (curryr hash-ref 'foo #f)
                         (lambda (h val) (hash-set h 'foo val))
                         (thunk 9))
       (hash 'foo 9)
       "when field not set, ensure-field-set takes its value from the thunk and sets it")

   (is (ensure-field-set (hash 'foo 7)
                         (curryr hash-ref 'foo #f)
                         (lambda (h val) (hash-set h 'foo val))
                         (thunk 9)
                         7)
       (hash 'foo 9)
       "can supply a value to determine what counts as 'unset'")
   ))

;;----------------------------------------------------------------------


(done-testing) ; this should be the last line in the file
