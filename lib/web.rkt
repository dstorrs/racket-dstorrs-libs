#lang racket

(require net/url
		 (planet neil/html-parsing:3:0))

(define/contract (is-local? s)
  (-> (or/c path-string? url?) boolean?) 
  (let ((scheme (url-scheme (cond
							 ((url? s)  s)
							 ((path? s) (string->url (path->string s)))
							 (else      (string->url s))))))
	(or (false? scheme)
		(equal? "file" scheme)
		(equal? 'file scheme)
		)))

(define/contract (to-url s)
  (-> (or/c path-string? url?) url?)
  (cond ((url? s)  s)
		((path? s) (string->url (path->string s)))
		(else (string->url s))))


(define/contract (web/call url-string
						   #:call-proc [call-proc port->string]
						   #:post-proc [post-proc html->xexp])
  (->* ((or/c path-string? url?))
	   (#:call-proc procedure? #:post-proc procedure?)
	   any)
  (post-proc
   (call/input-url (to-url url-string)
				   (curry get-pure-port #:redirections 5)
				   call-proc)))


(define/contract (get-page source 
				  #:as-text [as-text #f])
  (->* ((or/c path-string? url?))
	   (#:as-text boolean?)
	   (or/c string? list?))
  ((if as-text identity html->xexp) 
	((if (is-local? source)
		 (compose port->string open-input-file url->string to-url)
		 (curry web/call #:post-proc identity))
	 source)))

(provide (all-defined-out))