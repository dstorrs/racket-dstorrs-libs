#lang racket

(require racket/runtime-path
		 net/url
		 (planet neil/html-parsing:3:0)
		 "web.rkt"
		 "HTML-TreeBuilder.rkt"
		 )

(define latest-filings-url "https://www.sec.gov/cgi-bin/browse-edgar?company=&CIK=&owner=include&count=100&action=getcurrent")

(define latest-13Fs-url "https://www.sec.gov/cgi-bin/browse-edgar?company=&CIK=&type=13F&owner=include&count=100&action=getcurrent")

;;----------------------------------------------------------------------
;;    Get an xexp containing the 'tr's that represent the filings.
;;    Unfortunately, the SEC is using table-based layouts and no ids
;;    or classes.  There are five tables on the page, but only one
;;    div.  We want to get the second table from inside that div.
(define/contract (latest-filings-rows [url latest-filings-url])
  (->* () ((or/c path-string? url?)) list?)
  (look-down #:tag 'tr
   (second (look-down #:tag 'table
					  (look-down #:tag 'div
								 (get-page url))))))


(provide (all-defined-out))