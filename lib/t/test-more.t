#!/usr/bin/env racket

#lang racket
(require "../test-more.rkt")


(ok #t "#t works")
(ok (lambda () 9) "func returning 9 works.")
(displayln "\t### Next two tests deliberately have no message")
(ok 4)
(ok (lambda () 9))

;; (not-ok #f "not-ok #f works")
;; (not-ok (lambda () #f) "not-ok works w/ func returning #f.")
;; (displayln "\t### Next two tests deliberately have no message")
;; (not-ok #f)
;; (not-ok (lambda () #f))

(displayln "\t### Next 5 tests should say 'NOT ok'; second has no message")
(ok #f "this should say 'NOT ok', and so should the next one")
(ok #f)
(isnt 8 8 "this should say 'NOT ok' (isnt 8 8)")
(is 8 "8" "(is 8 \"8\") should say 'NOT ok'")
(unlike "baz" #px"foo" "/foo/ doesn't match foobar (test should say NOT ok)")

(displayln "\t### Should say 'ok' from here down; next test has no message, the rest do")
(isnt 3 "abc")
(is 8 8 "(is 8 8) works")
(is 8 8  eq? "(is 8 8) works when given eq?")

(isnt 7 "7" "(isnt 7 \"7\") works")

(not-ok (equal? 8 2) "8 does not equal 2")

(like "foobar" #px"foo" "/foo/ matches foobar")

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




;; ;;  @@TODO
;; ;; https://docs.racket-lang.org/overeasy/index.html
;; ;; - catch exceptions and report on them without terminating
;; ;; - specify the equal? op as something else
;; ;; - capture data from stdout and stderr, report on that
;; ;; - test groups
