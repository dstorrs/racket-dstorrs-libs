#!/usr/bin/env racket

#lang racket

(require "../test-more.rkt"
		 "../sec.rkt"
		 "../HTML-TreeBuilder.rkt"
		 racket/runtime-path
		 )

(define-runtime-path path "data/20160802_Latest_EDGAR_Filings.html")

(define standard-url "https://www.sec.gov/cgi-bin/browse-edgar?company=&CIK=&type=13f&owner=include&count=100&action=getcurrent")

(define second-url "https://www.sec.gov/cgi-bin/browse-edgar?company=&CIK=&type=13f&owner=include&count=40&action=getcurrent")

(define title-regex #px"Latest Filings Received and Processed at the SEC")

(define correct-text (port->string (open-input-file path)))

(ok 1 "test harness working")

(is url-13F-search standard-url "(url-13F-search) is correct")

(like (get-page #:source path #:as-text #t)
	  title-regex
	  "(get-page #:source path #:as-text #t)")

(let ((xlist (get-page #:source path)))
  (is (look-down xlist #:tag 'title)
	  '((title "Latest EDGAR Filings"))
	  "got the main page from file"))

(like (get-page #:as-text #t) title-regex "got the page from the web using implicit standard url")
(like (get-page #:source second-url #:as-text #t) title-regex "got the page from the web using specified url")

