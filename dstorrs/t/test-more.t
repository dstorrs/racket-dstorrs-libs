#!/usr/bin/env racket

#lang racket
(require "../test-more.rkt")
(require dstorrs/utils)

(define (inner-test thnk [should-pass #t])
  (define result #f)
  (with-output-to-string
    (thunk (set! result (thnk))))
  ((if should-pass tests-passed tests-failed) -1)   ; only count the outer test
  (_inc-test-num! -1) ; only count the outer test
  result)



(test-suite
 "simple 'ok' tests"

 (ok #t "#t works")
 (ok (lambda () 9) "func returning 9 works.")
 (displayln "\t### Next two tests deliberately have no message")
 (ok 4)
 (ok (lambda () 9))
 )

(test-suite
 "simple 'not-ok' and 'is-false' tests"

 (not-ok #f "not-ok #f works")
 (not-ok (lambda () #f) "not-ok works w/ func returning #f.")
 (not-ok (equal? 8 2) "8 does not equal 2")

 (is-false #f "is-false #f works")
 (is-false (lambda () #f) "is-false works w/ func returning #f.")
 (is-false (equal? 8 2) "is-false confirms that 8 does not equal 2")

 (displayln "\t### Next two tests deliberately have no message")
 (not-ok #f)
 (not-ok (lambda () #f))
 )

(test-suite
 "testing 'is'"

 (is 8 8 "(is 8 8) works")
 (is 8 8  "(is 8 8) works when given eq? as a positional arg" eq?)
 (is 8 8  "(is 8 8) works when given eq? as a keyword arg" #:op eq?)
 (is 8 #f  "(is 8 #f) works when given xor as a positional and a 'return false' as a keyword arg"
     xor
     #:op (lambda (x y) #f))
 )

(test-suite
 "testing 'isnt'"

 (isnt 7 "7" "(isnt 7 \"7\") works")
 (isnt (hash 'a 1 'b 2)
       7
       "a hash is not 7")

 (displayln "\t### Next test deliberately has no message")
 (isnt 3 "abc")

 (isnt 8 7  "(isnt 8 7) works when given (negate eq? as a positional arg"  (negate eq?))
 (isnt 8 8 #:op (negate xor)  "(isnt 8 8) works when given (negate xor) as a keyword arg")
 (isnt 8 #f  "(isnt 8 #f) works when given an 'or' func as a positional and a 'return false' as a keyword arg"
       (lambda (x y)  (or x y))
       #:op (lambda (x y)  #f))
 )

(define (test-gives-correct-output thnk regex msg [inc 0])
  (define output (with-output-to-string (thunk (thnk))))
  ;; (say "output is: '" output "'")
  ;; (say "regex is: '" regex "'")
  (_inc-test-num! -1) ; don't count the test inside the thunk
  (tests-failed inc)
  (ok (regexp-match regex output) msg)
  )

(when #t
  (test-suite
   "tests that fail give correct messages"

   (test-gives-correct-output
    (thunk (ok #f "ok msg"))
    (pregexp "^NOT ok \\d+ - ok msg\n$")
    "failed 'ok' test that has a msg reports correctly"
    -1
    )

   (test-gives-correct-output
    (thunk (ok #f))
    (pregexp "^NOT ok \\d+\n$")
    "failed 'ok' test that has no msg reports correctly"
    -1
    )

   (test-gives-correct-output
    (thunk (isnt 8 8 "isnt msg"))
    (pregexp "^NOT ok \\d+ - isnt msg\n  Got:\\s+8\n  Expected: \"<anything but 8>\"\n$")
    "failed 'isnt' test that has a msg reports correctly"
    -1
    )


   (test-gives-correct-output
    (thunk (isnt 8 8))
    (pregexp "NOT ok \\d+\n  Got:\\s+8\n  Expected: \"<anything but 8>\"\n$")
    "failed 'isnt' test that has no msg reports correctly"
    -1
    )
   (test-gives-correct-output
    (thunk (is 8 9 "is msg"))
    (pregexp "NOT ok \\d+ - is msg\n  Got:\\s+8\n  Expected: 9")
    "failed 'is' test that has a msg reports correctly"
    -1
    )

   (test-gives-correct-output
    (thunk (is 8 9))
    (pregexp "NOT ok \\d+\n  Got:\\s+8\n  Expected: 9")
    "failed 'is' test that has no msg reports correctly"
    -1
    )

   (test-gives-correct-output
    (thunk (unlike "foobar" #px"foo" "unlike msg"))
    (pregexp "NOT ok \\d+ - unlike msg\n  Got:\\s+\"foobar\"\n  Expected: \"<something NOT matching #px[^\"]+\"foo[^\"]+\"")
    "failed 'unlike' test that has a msg reports correctly"
    -1
    )

   (test-gives-correct-output
    (thunk (unlike "foobar" #px"foo"))
    (pregexp "NOT ok \\d+\n\\s+Got:\\s+\"foobar\"\n  Expected: \"<something NOT matching #px[^\"]\"foo[^\"]+\">")
    "failed 'unlike' test that has no msg reports correctly"
    -1
    )

   ); test-suite
  ); when

(test-suite
 "'like' and 'unlike' work"

 (like "foobar" #px"foo" "/foo/ matches foobar")
 (unlike "foobar" #px"baz" "/baz/ correctly does NOT match foobar")
 )

(test-suite
 "'throws' works"

 (define (boom) (raise-argument-error 'boom "PEBKAC" 18))

 (define str
   (with-output-to-string
     (thunk
      (throws (thunk "doesn't throw") "this string not used"))))
 (tests-failed -1)   ; undo the inner test above -- it was supposed to fail.
 (_inc-test-num! -1) ; undo the inner test above -- it was supposed to fail.

 (like str
       #px"NOT ok.+?\\[DID NOT THROW\\]"
       "'throws' correctly handles it when it doesn't throw")

 (throws boom
         exn:fail:contract?
         "(throws) can use builtin func ")

 (throws boom
         (lambda (x) (exn:fail:contract? x))
         "(throws) can use my own func")

 (throws  boom
          "PEBKAC"
          "(throws): exception has the expected message (match by string)")

 (throws boom
         #px"KAC"
         "(throws): exception has the expected message (match by regex)")


 (throws (thunk (raise 'foo))
         'foo
         "'throws' notices if you throw a symbol instead of an exn? object")

 (throws (thunk (raise 7))
         7
         "'throws' will notice if you raise a number instead of an exn? object")

 (throws (thunk (raise "foo"))
         "foo"
         "'throws' will notice if you raise a string instead of an exn? object")

 (throws (thunk (raise #t))
         #t
         "'throws' will notice if you raise #t instead of an exn? object")

 );test-suite

(test-suite
 "'dies' handles non-exn values"

 (dies (thunk (raise-argument-error 'foo "bar" 8))
       "'dies' doesn't care why it died, just that it did.")

 (dies (thunk (raise 7))
       "'dies' will notice if you raise a number instead of an exn? object")

 (dies (thunk (raise "foo"))
       "'dies' will notice if you raise a string instead of an exn? object")

 (dies (thunk (raise #t))
       "'dies' will notice if you raise #t instead of an exn? object")
 )

(test-suite
 "is-type/isnt-type, matches/not-matches"

 (define (check-it funcs val pred)
   (for ((func funcs))
     (is (func val pred (format "(~a ~a ~a)" (object-name func) val  (object-name pred)))
         val
         (format "(~a ~a) returns ~a" (object-name func) val val))))

 (define check-is-good (curry check-it (list is-type matches)))

 (define greater-than-one? (lambda (x) (> x 1)))
 (check-is-good  7 greater-than-one?)
 (check-is-good  7 integer?)
 (check-is-good "foo" string?)
 (check-is-good 'foo symbol?)
 (check-is-good (hash) hash?)
 (check-is-good (vector) vector?)

 (define check-is-bad  (curry check-it (list isnt-type not-matches)))
 (check-is-bad  0 greater-than-one?)
 (check-is-bad  -7 exact-positive-integer?)
 (check-is-bad "foo" symbol?)
 (check-is-bad 'foo string?)
 (check-is-bad (hash) vector?)
 (check-is-bad (vector) hash?)

 )

(test-suite
 "tests return their final value"

 (is (inner-test (thunk (lives (thunk 8.2)) 8.2))
     8.2
     "Got the correct value from lives")

 (is (inner-test (thunk (dies (thunk (raise "foo")))))
     "foo"
     "Got the correct value from dies")

 (is (inner-test (thunk
                  (throws (thunk (raise "8.2"))
                          #px"8.2")))
     "8.2"
     "Got the correct value from throws")

 (is (inner-test (thunk (ok 8.2)))
     8.2
     "Got the correct value from ok")


 (is (inner-test (thunk (not-ok #f)))
     #t
     "Got the correct value from ok")

 (is (inner-test (thunk (is 8.2 8.2)))
     8.2
     "Got the correct value from is")

 (is (inner-test (thunk (isnt 8.2 9)))
     8.2
     "Got the correct value from isnt")

 (is (inner-test (thunk (is-type 8 exact-positive-integer?)))
     8
     "Got the correct value from is-type")

 (is (inner-test (thunk (like "foo" #px"foo")))
     '("foo")
     "Got the correct value from like")

 (is (inner-test (thunk (unlike "foo" #px"bar")))
     "foo"
     "Got the correct value from unlike")

 );test-suite

(test-suite
 "make-test-file"

 (define filepath (build-path "/tmp/test-file-foo"))

 (define (delete-test-file)
   (with-handlers ((exn:fail? identity)) ; ignore exceptions
     (delete-file filepath)))

 (delete-test-file)
 (not-ok (file-exists? filepath) "before creation, file does not exist")

 (lives (thunk (make-test-file filepath)) "make-test-file lived") 

 (delete-test-file)

 (not-ok (file-exists? filepath) "file successfully removed")
 (lives (thunk (make-test-file filepath "this is the right text"))
        "lived: make-test-file filepath with specified text"
        )

 (ok (file-exists? filepath) "file created")

 (is (with-input-from-file filepath port->string)
     "this is the right text"
     "file was correctly populated")

 (throws (thunk (make-test-file filepath "this is new text" #:overwrite #f))
         #px"file exists"
         "make-test-file dies if the file exists and you say '#:overwrite #f'"
         )
 (let ((the-path (lives (thunk (make-test-file "/tmp")) "made test file")))
   (ok (file-exists? the-path) "a random filename was generated and returned"))

 (let ((the-path "/tmp/jasdlfhadkhkwhkdhfgjljlzdldjoe/foo"))
   (ok (not (directory-exists? "/tmp/jasdlfhadkhkwhkdhfgjljlzdldjoe"))
       "about to run make-test-file with a directory that doesn't exist.  Fortunately, the directory really doesn't exist")
   (lives (thunk (make-test-file the-path)) "make-test-file with a path to a non-existent dir lives")
   )
 )

(test-suite
 "lives"
 (lives (thunk 7) "lives succeeds if given a number")
 (lives (thunk "foo") "lives succeeds if given a string")
 (lives (thunk 'foo) "lives succeeds if given a symbol")

 (define (do-test thnk type)
   (with-handlers (( (lambda (e) #t) ; catch everything
                     (lambda (e)
                       (ok #f (~a "throws failed to catch a " type))
                       )))
     (void (with-output-to-string thnk))
     ; If we get to here then 'lives' caught the error
     (tests-failed -1)    ; Don't count 'lives' as an actual failure
     (_inc-test-num! -1)  ; In fact, don't count it at all
     (ok #t (~a "lives caught a " type))))


 (do-test (thunk (lives (thunk (raise "foo"))))
          "string")

 (do-test (thunk (lives (thunk (raise 8))))
          "number")

 (do-test (thunk (lives (thunk (raise-argument-error 'foo "foo" 7))))
          "raise-argument-error exception")
 )

(when #t
  (test-suite
   "is-approx and isnt-approx"

   (is-approx 1 1 "1 is approximately 1")
   (is-approx 1 0 "by default, 1 is approximately 0")
   (is-approx 1 2 "by default, 1 is approximately 2")
   (is-approx 1 1.5 "by default, 1 is approximately 1.5")

   (isnt-approx 1 2.5 "by default, 1 is NOT approximately 2.5")
   (isnt-approx 1 -1 "by default, 1 is NOT approximately -1")

   (isnt-approx 1 -5 "(isnt-approx 1 -5)")
   (is-approx 1 -5 #:threshold 10 "1 is approx -5 if #:threshold is 10")

   (throws (thunk (is-approx '(a b c) '(d e f)))
           #px"arguments to is-approx / isnt-approx must be numeric or you must include a #:with function to return an exact numeric measurement from 'got' and 'expected'"
           "throws when given non-numeric and no #:with arg")

   (is-approx '(a b c) '(d e f) #:with length "(is-approx '(a b c) '(d e f) #:with length)")
   (is-approx '(a b c) '(d e f g) #:with length "(is-approx '(a b c) '(d e f g) #:with length), since default threshold is 1")
   (isnt-approx '(a b c) '(d e f g) #:with length #:threshold 0 "(isnt-approx '(a b c) '(d e f g) #:with length #:threshold 0)")

   (is (is-approx '(a b c) '(d e f g h) #:threshold 10 #:with length
                  "(is-approx '(a b c) '(d e f g h) #:threshold 10 #:with length)")
       2
       "(is-approx '(a b c) '(d e f g) #:with length #:threshold 10) returns 2")

   (define now (current-seconds)) ; epoch time
   (is-approx  (and (sleep 1) (current-seconds)) now "(myfunc) took no more than 1 second")

   (let ([myfunc (thunk (make-list 7 'x))])
     (is-approx  (length (myfunc)) 8 "(myfunc) returned a list of about 8 elements"))

   (for ([num (in-range 7)])
     (let ([myfunc (thunk (make-list num 'x))])
       (is-approx  (length (myfunc)) 3 #:threshold 3 (~a "(myfunc) returned list of " num " elements, which is in the expected range (0-6)"))))

   (for ([num (in-range 3 7)])
     (let ([myfunc (thunk (make-list num 'x))])
       (is-approx  (length (myfunc))
                   6
                   #:threshold 3
                   #:abs-diff? #f
                   "(myfunc) => list of 3-6 elements")))

   (is-approx  (hash 'age 8)
               (hash 'age 9)
               #:with (curryr hash-ref 'age)
               "age is about 9")

   ;  The following is a silly example but it shows some of the versatility
   (is-approx  ((thunk "Foobar"))
               "f"
               #:with (compose1 char->integer (curryr string-ref 0) string-downcase)
               #:abs-diff? #f
               "(myfunc) returns a string that starts with 'f', 'F', 'g', or 'G'")

   )
  )

;;  @@TODO
;; https://docs.racket-lang.org/overeasy/index.html
;; - capture data from stdout and stderr, report on that

(done-testing) ; this should be the last line in the file
