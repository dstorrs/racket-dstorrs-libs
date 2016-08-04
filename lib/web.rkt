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


;;----------------------------------------------------------------------
;;    Get a page from the internet. Accepts a path, string, or url.
;;    'call-proc' gets passed to call/input-url.  'post-proc' gets the
;;    result of call/input-url
;;
(define/contract (web/call url-string
						   #:call-proc [call-proc port->string]
						   #:post-proc [post-proc html->xexp]
						   #:as-text   [as-text #f]) ;; Really just a convenient shortcut
  (->* ((or/c path-string? url?))
	   (#:call-proc (-> input-port? any)
		#:post-proc (-> any/c any)
		#:as-text boolean?)
	   any)
  ((if as-text identity post-proc)  
   (call/input-url (to-url url-string)
				   (curry get-pure-port #:redirections 5)
				   call-proc))) ;; Note that if you passed #:as-text, this better return a string


;;--------------------------------------------------------------------------------
;;    Get a page from the internet (via web/call) or from a
;;    file. 'post-proc' will be run across the results.
(define/contract (get-page source 
				  #:post-proc [post-proc html->xexp]
				  #:as-text   [as-text #f]) ; easier to remember than '#:post-proc identity'
  (->* ((or/c path-string? url?))
	   (#:post-proc (-> string? any/c)
	    #:as-text boolean?)
	   (or/c string? list?))
  ((if as-text identity post-proc)
	((if (is-local? source)
		 (compose port->string open-input-file url->string to-url)
		 (curry web/call #:as-text #t))
	 source)))

(provide (all-defined-out))