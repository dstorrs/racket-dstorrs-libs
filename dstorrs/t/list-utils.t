#!/usr/bin/env racket

#lang racket

(require "../list-utils.rkt"
		 "../test-more.rkt")

(ok 1 "test harness is working")

(define l '(foo
			"bar"
			("baz" (quux))))

(is (car l) 'foo "car l is 'foo")
(is (-> l '(0)) 'foo "(-> l '(0)) is 'foo")
(is (-> l '(1)) "bar" "(-> l '(1)) is \"bar\"")
(is (-> l '(2)) '("baz" (quux)) "(-> l '(2)) is '(\"baz\" (quux))")
(is (-> l '(2 0)) "baz" "(-> l '(2 0)) is '\"baz\"")
(throws (lambda () (-> l '(188 0)))
		#px"list-ref: index too large for list"
		"(-> l '(188 0)) throws: index too large for list")




(define h (make-hash
		   `(("foo" . "bar")
			 (baz . 7)
			 (quux . (foo bar))
			 (blag . ,(make-hash '(["baz" . "jaz"]))))))

(for ((k '("foo" baz quux blag (blag "baz")))
	  (v `("bar" 7 (foo bar) ,(make-hash '(["baz" . "jaz"])))))
	 (is (-> h `(,k)) v (format "(-> h ~a) is ~a" k v)))

(is (-> h '(quux 0)) 'foo "(-> h '(quux 0) is foo")

(throws (lambda () (-> h '(jaz)))
		#px"no value found for key"
		"throws on non-existent key 'jaz")
(throws (lambda () (-> h '(blag jaz)))
		#px"no value found for key"
		"throws on non-existent key '(blag jaz)")

(is (autobox "foo") '("foo") "(autobox \"foo\" returns '(\"foo\")")
(is (autobox '("foo")) '("foo") "(autobox '(\"foo\") returns '(\"foo\")")

(is (atom? "foo") #t "atom? detects string as atom")
(is (atom? 7) #t "atom? detects string as atom")
(is (atom? (make-hash '((a . 7)))) #t "atom? detects hash as atom")
(is (atom? '("foo")) #f "atom? detects list as non-atom")

(is (remove-nulls '(foo bar)) '(foo bar) "remove-nulls leaves list unchanged if it contains no null list")
(is (remove-nulls '(foo () bar)) '(foo bar) "remove-nulls removes one null")
(is (remove-nulls '(foo (()) bar)) '(foo (()) bar) "remove-nulls does not remove (())")

(for ((v `(#f #t "foo" 7 list? () ,(make-hash '((x . 7))) ,(make-vector 8))))
	 (not-ok (list-not-null? v) (format "list-not-null? ~a is #f" v)))

(for ((v '((foo) (()))))
	 (ok (list-not-null? v) (format "(list-not-null? ~a is #t" v)))

