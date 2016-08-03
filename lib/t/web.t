#!/usr/bin/env racket

#lang racket

(require net/url
		 "../web.rkt"
		 "../test-more.rkt")


(for ((p `("sec.t"
		   "file://./sec.t"
		   ,(string->path "sec.t")
		   )))
	 (ok (is-local? p) (format "(is-local? ~a) is #t" p)))

(for ((p `( "http://www.google.com"
			,(string->url "http://www.google.com"))))
	 (not-ok (is-local? p) (format "(is-local? ~a) is #f" p)))



