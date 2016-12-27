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

(test-suite
 "tests that should fail"

 (displayln "\t### Next 5 tests should say 'NOT ok'; second has no message")
 (ok #f "(ok) with message.  Next one is (ok) without message")
 (ok #f)
 (isnt 8 8 "this should say 'NOT ok' (isnt 8 8)")
 (is 8 "8" "(is 8 \"8\") should say 'NOT ok'")
 (unlike "foobar" #px"foo" "/foo/ doesn't match foobar (test should say NOT ok)")
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
