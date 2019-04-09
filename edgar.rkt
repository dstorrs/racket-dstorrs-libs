#!/usr/bin/env racket
 
#lang racket

;;    This is some one-off code for accessing the SEC's EDGAR database
;;    of corporate filings.  It's usable but single-purpose and not
;;    terribly robust.

(require "HTML-TreeBuilder.rkt"
		 "list-utils.rkt"
		 threading
		 )

(define latest-filings-url "https://www.sec.gov/cgi-bin/browse-edgar?company=&CIK=&owner=include&count=10&action=getcurrent");


;; #    The SEC does table-based layouts, and there's no IDs or
;; #    CLASSes to make finding things easier.  There's only one DIV,
;; #    and inside it are 5 tables.  We want the fourth one.

(define (make-rec l)
  (if (null? l)
	  null
	  (let* ((f (first l))
			 (s (second l))
			 (l (first-hlink f))
			 (text (hlink-text l))
			 (url  (hlink-url  l))
			 (matches    (regexp-match #px"^(.+?)\\s+\\((\\d+)\\)\\s.+$" text)
						 ))
		(hash 'name-filed  text
			  'name        (second matches)
			  'cik         (third matches)
			  'all-filings (hlink-url l)
			  'doc-hlink   (hlink-url (first-hlink s))))))


  (~> (html-treebuilder-new latest-filings-url)
	  (look-down #:tag 'div)
	  (look-down #:tag 'table)
	  (list-ref 1)
	  (look-down #:tag 'tr)
	  rest                      ;; drop the header row
	  (step-by-n make-rec _)
	  (filter hash? _)
	  )


