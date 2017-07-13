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
 (is 8 8  "(is 8 8) works when given eq?" eq?)
 )

(test-suite
 "testing 'isnt'"

 (isnt 7 "7" "(isnt 7 \"7\") works")
 (isnt (hash 'a 1 'b 2)
       7
       "a hash is not 7")

 (displayln "\t### Next test deliberately has no message")
 (isnt 3 "abc")
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
    (pregexp "^NOT ok \\d+ - isnt msg\n  Got:\\s+8\n  Expected: <anything but 8>\n$")
    "failed 'isnt' test that has a msg reports correctly"
    -1
    )


   (test-gives-correct-output
    (thunk (isnt 8 8))
    (pregexp "NOT ok \\d+\n  Got:\\s+8\n  Expected: <anything but 8>\n$")
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
    (pregexp "NOT ok \\d+ - unlike msg\n  Got:\\s+foobar\n  Expected: <something NOT matching #px\"foo\">")
    "failed 'unlike' test that has a msg reports correctly"
    -1
    )

   (test-gives-correct-output
    (thunk (unlike "foobar" #px"foo"))
    (pregexp "NOT ok \\d+\n  Got:\\s+foobar\n  Expected: <something NOT matching #px\"foo\">")
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

 (lives (thunk (make-test-file filepath)) "after creation, file exists")

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

 )
 
;;  @@TODO
;; https://docs.racket-lang.org/overeasy/index.html
;; - capture data from stdout and stderr, report on that

(done-testing) ; this should be the last line in the file
