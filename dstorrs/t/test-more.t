#!/usr/bin/env racket

#lang racket
(require "../test-more.rkt")

(test-suite
 "simple 'ok' tests"

 (ok #t "#t works")
 (ok (lambda () 9) "func returning 9 works.")
 (displayln "\t### Next two tests deliberately have no message")
 (ok 4)
 (ok (lambda () 9))
 )

(test-suite
 "simple 'not-ok' tests"

 (not-ok #f "not-ok #f works")
 (not-ok (lambda () #f) "not-ok works w/ func returning #f.")
 (not-ok (equal? 8 2) "8 does not equal 2")

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

(define (test-gives-correct-output thnk regex msg)
  (define output (with-output-to-string (thunk (thnk))))
  (like output regex msg)
  )

(test-suite
 "tests that fail give correct messages"

 (test-gives-correct-output
  (thunk (ok #f "ok msg"))
  (pregexp "NOT ok \\d+ - ok msg")
  "failed 'ok' test that has a msg reports correctly")
 
 (test-gives-correct-output
  (thunk (ok #f))
  (pregexp "NOT ok \\d+")
  "failed 'ok' test that has no msg reports correctly")
 
 (test-gives-correct-output
  (thunk (isnt 8 8 "isnt msg"))
  (pregexp "NOT ok \\d+ - isnt msg")
  "failed 'isnt' test that has a msg reports correctly")
 
 (test-gives-correct-output
  (thunk (isnt 8 8))
  (pregexp "NOT ok \\d+")
  "failed 'isnt' test that has no msg reports correctly")
 
 (test-gives-correct-output
  (thunk (is 8 9 "is msg"))
  (pregexp "NOT ok \\d+ - is msg")
  "failed 'is' test that has a msg reports correctly")
 
 (test-gives-correct-output
  (thunk (is 8 9))
  (pregexp "NOT ok \\d+")
  "failed 'is' test that has no msg reports correctly")
 
 (test-gives-correct-output
  (thunk (unlike "foobar" #px"foo" "unlike msg"))
  (pregexp "NOT ok \\d+ - unlike msg")
  "failed 'unlike' test that has a msg reports correctly")
 
 (test-gives-correct-output
  (thunk (unlike "foobar" #px"foo"))
  (pregexp "NOT ok \\d+")
  "failed 'unlike' test that has no msg reports correctly")
 
 (tests-failed -8) ; reset the counter so it doesn't double-count the ones inside the tests above
 )

(test-suite
 "'like' and 'unlike' work"

 (like "foobar" #px"foo" "/foo/ matches foobar")
 (unlike "foobar" #px"baz" "/baz/ correctly does NOT match foobar")
 )

(test-suite
 "'throws' and 'dies' work"
 
 (define (boom) (raise-argument-error 'boom "PEBKAC" 18))
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

 (dies boom
       "'dies' doesn't care why it died, just that it did.")
)
 

 ;; ;;  @@TODO
 ;; ;; https://docs.racket-lang.org/overeasy/index.html
 ;; ;; - catch exceptions and report on them without terminating
 ;; ;; - specify the equal? op as something else
 ;; ;; - capture data from stdout and stderr, report on that
 ;; ;; - test groups
