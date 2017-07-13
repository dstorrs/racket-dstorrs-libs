#!/usr/bin/env racket

#lang racket

(require "../test-more.rkt"
		 "../sec.rkt"
		 "../HTML-TreeBuilder.rkt"
		 racket/runtime-path
		 net/url
		 )

(define correct-header-row
  '(tr (@ (bgcolor "#D6D6D6")) "\n" (th (@ (align "left") (nowrap "nowrap") (width "4%")) "Form") "\n" (th (@ (align "left") (nowrap "nowrap") (width "4%")) "Formats") "\n" (th (@ (align "left") (width "40%")) "Description") "\n" (th (@ (align "left") (nowrap "nowrap") (width "4%")) "Accepted") "\n" (th (@ (align "left") (nowrap "nowrap") (width "4%")) "Filing Date") "\n" (th (@ (align "left") (nowrap "nowrap") (width "4%")) "File/Film No") "\n"))

(define-runtime-path main-path  "data/20160802_Latest_EDGAR_Filings/main_page.html")

(define new-filings-url "https://www.sec.gov/cgi-bin/browse-edgar?company=&CIK=&owner=include&count=100&action=getcurrent")
(define new-13Fs-url    "https://www.sec.gov/cgi-bin/browse-edgar?company=&CIK=&type=13F&owner=include&count=100&action=getcurrent")
(define second-url "https://www.sec.gov/cgi-bin/browse-edgar?company=&CIK=&type=13f&owner=include&count=40&action=getcurrent")
(define title-regex #px"Latest Filings Received and Processed at the SEC")
(define correct-text (port->string (open-input-file main-path)))

(ok 1 "test harness working")

(is latest-filings-url new-filings-url "(latest-filings-url) is correct")
(is latest-13Fs-url    new-13Fs-url    "(latest-13Fs-url) is correct")

;; (let ((rows (latest-filings-rows-raw)))
;;   (is (car rows) correct-header-row "got correct header row from table")
;;   (is (length rows) 201 "got 201 raw rows (header + 100 records w/2 rows each) of filings"))

;; (let ((rows (latest-filings-rows)))
;;   (is (length rows) 100 "got 100 post-processed rows of filings")
;;   (for ((h (take rows 10))
;; 		(num (in-naturals)))
;; 	   (ok (hash? h) (format "row #~a is a hash" num))
;; 	   (is (sort (hash-keys h) string<?) '("cik" "doc-link" "name") "got expected keys")
;; 	   (ok (andmap
;; 			(lambda (s) (and (string? s) (length s)) (hash-values h)))
;; 		   "all values are non-empty strings")
;; 	   ))


(done-testing)
