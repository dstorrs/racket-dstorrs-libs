#!/usr/bin/env racket

#lang at-exp rackjure

(require dstorrs/utils
		 dstorrs/test-more
		 )

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
 
 (is (safe-hash-remove (hash-mut) 'a)
     (make-hash '((b . 2) (c . 3)))
     "(safe-hash-remove hash-mut 'a) worked")

 (is (safe-hash-remove (hash-mut) 'x)
     (make-hash '((a . 1) (b . 2) (c . 3)))
     "(safe-hash-remove hash-mut 'x) worked")
 );; test-suite

(test-suite
 "safe-hash-set"
 (define hash-imm (hash 'a 1 'b 2 'c 3))
 (ok (immutable? hash-imm) "using immutable hash for next test")
 (is (safe-hash-set hash-imm 'b 5)
     (hash 'a 1 'b 5 'c 3)
     "can handle an immutable hash")

 (define hash-mut (make-hash '((a . 1) (b . 2) (c . 3))))
 (define correct  (make-hash '((a . 1) (b . 5) (c . 3))))
 (ok (not (immutable? hash-mut)) "using mutable hash for next test")
 (ok (not (immutable? correct)) "using mutable hash for answer to next test")
 (is (safe-hash-set hash-mut 'b 5)
     correct
     "can handle a mutable hash")

 (dies (thunk (safe-hash-set #f 'a 7))
       "safe-hash-set requires a hash")
 )

(test-suite
 "verify-struct"

 (struct foo (a b c))
 (define x (foo 1 2 3))
 (define y (foo 0 2 3))
 
 (is-type x foo? "x is a foo")
 (is-type y foo? "y is a foo")

 (isnt x y "x and y are not equal?")
 (ok (thunk (verify-struct #:struct x
                           #:funcs (list foo-b foo-c)
                           #:expected  (list 2 3)))
     "validates when given explicit values")
 
 (ok (thunk (verify-struct #:struct x
                           #:funcs (list foo-b foo-c)
                           #:expected  y))
     "validates when given a comparison struct")
  
 (not-ok (thunk (verify-struct #:struct x
                               #:funcs (list foo-a foo-b foo-c)
                               #:expected  y))
         "correctly reports that they are not equal if you have it check a field that is not equal")
 
 )

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
         "dir-and-filename throws if given a one-element relative path WITHOUT training slash ('foo')") 
 
 (throws (thunk (dir-and-filename "foo/"))
         #rx"Cannot accept single-element relative paths"
         "dir-and-filename throws if given a one-element relative path WITH a training slash ('foo/')")

 (throws (thunk (dir-and-filename "/"))
         #rx"Cannot accept root path"
         "dir-and-filename throws if given the root path")

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
(say "Done testing.")
