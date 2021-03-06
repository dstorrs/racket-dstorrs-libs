#!/usr/bin/env racket

#lang racket/base

(require net/url
         racket/function
         racket/port
         racket/runtime-path
         "../HTML-Element.rkt"
         "../test-more.rkt"
         "../web.rkt")

(expect-n-tests 28)

;;    Constants for tests
(define-runtime-path main-path  "data/20160802_Latest_EDGAR_Filings/main_page.html")
(define standard-url "https://www.sec.gov/cgi-bin/browse-edgar?company=&CIK=&type=13f&owner=include&count=100&action=getcurrent")
(define second-url "https://www.sec.gov/cgi-bin/browse-edgar?company=&CIK=&type=13f&owner=include&count=40&action=getcurrent")
(define title-regex #px"Latest Filings Received and Processed at the SEC")
(define correct-text (port->string (open-input-file main-path)))


(test-suite
 "to-url"
 (ok 1 "test harness is working")

 (ok (path? main-path) "main-path is a path, not a string")
 (ok (string? (path->string main-path)) "(path->string main-path) is a string")
 (ok (url? (string->url (path->string main-path))) "(string->url path->string main-path) is a url")
 (ok (url? (to-url main-path)) "to-url accepts paths")
 (ok (url? (to-url (path->string main-path))) "to-url accepts strings")
 (ok (url? (to-url (string->url (path->string main-path)))) "to-url accepts urls")

 (like (url->string (to-url main-path))
       #px"^file://"
       "to-url accepts a path and returns a file:// url"
       )

 (like (url->string (to-url #:treat-string-as-path? #t (path->string main-path)))
       #px"^file://"
       "(to-url #:treat-string-as-path? #t (path->string main-path)) returns a file:// url"
       )
 )

(test-suite
 "is-local?"
 (for ((p `("sec.t"
            "file://./sec.t"
            ,(string->path "sec.t")
            )))
   (ok (is-local? p) (format "(is-local? ~a) is #t" p)))

 (for ((p `( "http://www.google.com"
             ,(string->url "http://www.google.com"))))
   (not-ok (is-local? p) (format "(is-local? ~a) is #f" p)))
 )

(test-suite
 "get-page"
 (like (get-page main-path #:as-text #t)
       title-regex
       "(get-page main-path #:as-text #t)")
 (for ((f (list identity path->string to-url))
       (t '(path string url)))
   (like (get-page (f main-path) #:as-text #t)
         title-regex
         (format "get-page accepts ~a" t)))

 (is (look-down (get-page main-path) #:tag 'title)
     '((title "Latest EDGAR Filings"))
     "got the main page from file as xexp")


 (lives (lambda ()
          (like (get-page second-url #:as-text #t)
                title-regex
                "got the page from the web using second url"))
        "get-page lived")
 )

(test-suite
 "web/call"
 (lives (lambda ()
          (ok (let ((res (web/call standard-url)))
                (and (list? res) (not (null? res))))
              "web/call got the page from the net as xexp"))
        "web/call lived")

 (ok (let ((res (web/call main-path)))
       (and (list? res) (not (null? res))))
     "web/call got the page from the disk as xexp")
 )


