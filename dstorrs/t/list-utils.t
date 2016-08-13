#!/usr/bin/env racket

#lang racket

(require "../list-utils.rkt"
		 "../test-more.rkt")

(ok 1 "test harness is working")

(define l '(foo "bar" ("baz" (quux))))
(define h (make-hash
		   `(("foo" . "bar")
			 (baz . 7)
			 (quux . (foo bar))
			 (blag . ,(make-hash '(["baz" . "jaz"]))))))

(is (car l) 'foo "car l is 'foo")
(is (get l 0) 'foo "(get l 0) is 'foo")
(is (get l '(0)) 'foo "(get l '(0)) is 'foo")
(is (get l '(1)) "bar" "(get l '(1)) is \"bar\"")
(is (get l '(2)) '("baz" (quux)) "(get l '(2)) is '(\"baz\" (quux))")
(is (get l '(2 0)) "baz" "(get l '(2 0)) is '\"baz\"")
(throws (lambda () (get l '(188 0)))
		#px"list-ref: index too large for list"
		"(get l '(188 0)) throws: index too large for list")
(is (get l '(188) -11) -11
	"(get l '(188 0) -11) returns -11; the index was too big so it defaulted")

(for ((k '("foo" baz quux blag (blag "baz")))
	  (v `("bar" 7 (foo bar) ,(make-hash '(["baz" . "jaz"])))))
	 (is (get h `(,k)) v (format "(get h ~a) is ~a" k v)))

(is (get h 'quux) '(foo bar) "(get h '(quux 0) is '(foo bar)")
(is (get h '(quux 0)) 'foo "(get h '(quux 0) is foo")

(throws (lambda () (get h '(jaz)))
		#px"no value found for key"
		"throws on non-existent key 'jaz")
(throws (lambda () (get h '(blag jaz)))
		#px"no value found for key"
		"throws on non-existent key '(blag jaz)")

(is (get h '(jaz) "not found")
	"not found"
	"defaults correctly if key was not found in hash")

(is (autobox "foo") '("foo") "(autobox \"foo\" returns '(\"foo\")")
(is (autobox '("foo")) '("foo") "(autobox '(\"foo\") returns '(\"foo\")")

(is (atom? "foo") #t "atom? detects string as atom")
(is (atom? 7) #t "atom? detects string as atom")
(is (atom? (hash 'a 7)) #t "atom? detects hash as atom")
(is (atom? '("foo")) #f "atom? detects list as non-atom")

(is (remove-nulls '(foo bar)) '(foo bar) "remove-nulls leaves list unchanged if it contains no null list")
(is (remove-nulls '(foo () bar)) '(foo bar) "remove-nulls removes one null")
(is (remove-nulls '(foo (()) bar)) '(foo (()) bar) "remove-nulls does not remove (())")

(for ((v `(#f #t "foo" 7 list? () ,(make-hash '((x . 7))) ,(make-vector 8))))
	 (not-ok (list/not-null? v) (format "list/not-null? ~a is #f" v)))

(for ((v '((foo) (()))))
	 (ok (list/not-null? v) (format "(list/not-null? ~a is #t" v)))

(is (get '(a b c) '(0)) 'a)