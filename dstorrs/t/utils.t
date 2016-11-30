#!/usr/bin/env racket

#lang at-exp rackjure

(require dstorrs/utils
		 dstorrs/test-more
		 )

(ok #t "testing harness works")

(for ((v (list 0 0.0 -0 -0.0 "" #f (void) null)))
	 (ok (perl-false? v) @~a{@v is false}))


(say "Done testing.")

