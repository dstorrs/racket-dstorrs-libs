#lang racket/base

(require html-parsing
         net/url
         racket/bool
         racket/contract/base
         racket/contract/region
         racket/function
         racket/port
         racket/system)

(provide is-local?         ; given a url-ish thing, determine if it's a local resource
         to-url            ; convert to a url
         ->absolute-url    ; generate an absolute url from base and component
         web/call          ; get a page from the internet with a lot of processing options
         fetch-with-curl   ; shell out to curl in order to fetch HTTPS pages on OSX (ARGH!)
         get-page          ; more readable but less flexible wrapper around web/call
         )

;;----------------------------------------------------------------------
;;    Check if a url-ish thing refers to a local resource or one on
;;    the net.
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

;;----------------------------------------------------------------------
;;    Turn a url-ish thing (string, path, url) into an url
(define/contract (to-url s)
  (-> (or/c string? path-string? url?) url?)
  (cond ((url? s)  s)
        ((path? s) (string->url (path->string s)))
        (else (string->url s))))

(define (url-as-string s) (url->string (to-url s)))

;;----------------------------------------------------------------------
;;    Two args, 'b' and 'u'.  Both can be path, string, or url.  If
;;    'u' is absolute, return it.  If it is relative, combine it with
;;    'b'.  In either case, the return value is a url structure
;;    regardless of how it came in.
(define/contract (->absolute-url b u)
  (->(or/c string? path-string? url?) (or/c string? path-string? url?) url?)
  (let ((u (to-url u)))
    (if (url-scheme u) u (combine-url/relative (to-url b) (url->string u)))))

;;----------------------------------------------------------------------
;;    Get a page from the internet. Accepts a path, string, or url.
;;    'call-proc' gets passed to call/input-url.  'post-proc' gets the
;;    result of call/input-url.  Returns the result of post-proc.  By
;;    default returns an xexp representing the page.
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

(define (fetch-with-curl url)
  ;; OSX El Capitan won't talk to HTTPS sites because it ships with
  ;; out of date libcrypto and libssl libraries and won't let you
  ;; update them.  (Seriously, root isn't allowed to update them.)
  ;; Default to this stupid hack.
  (html->xexp
   (with-output-to-string
     (lambda () (system (format "curl ~a" (url-as-string url)))))))

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

