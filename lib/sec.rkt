#lang racket

(require racket/runtime-path
		 net/url
		 "web.rkt"
		 (planet neil/html-parsing:3:0)
		 )

(define url-13F-search "https://www.sec.gov/cgi-bin/browse-edgar?company=&CIK=&type=13f&owner=include&count=100&action=getcurrent")

;;    
(define (get-page #:source  [source url-13F-search]
					   #:as-text [as-text #f])
  (->* ()
	   (#:source  (or/c string? path-string? url?)
				  #:as-text boolean?)
	   (or/c string? list?))
  (let* ((u (to-url source))
		 (str (port->string (if (is-local? source)
								(open-input-file (url->string u))
								(get-pure-port  u)))))
	(if as-text str (html->xexp str))))

(provide (all-defined-out))