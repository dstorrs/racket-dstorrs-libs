#lang racket

(require net/url
		 (planet neil/html-parsing:3:0))

(define (is-local? s)
  ((or/c path-string? url?) boolean?)
  (let ((scheme (url-scheme (cond
							 ((url? s)  s)
							 ((path? s) (string->url (path->string s)))
							 (else      (string->url s))))))
	(or (false? scheme)
		(equal? "file" scheme)
		(equal? 'file scheme)
		)))

(define (to-url s)
  (-> (or/c path-string? url?) url?)
  (cond ((url? s)  s)
		((path? s) (string->url (path->string s)))
		(else (string->url s))))

(define (web/call url-string)
  (html->xexp
   (call/input-url (string->url url-string)
				   (curry get-pure-port #:redirections 5)
				   port->string)))

(provide (all-defined-out))