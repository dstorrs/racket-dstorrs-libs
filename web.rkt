#lang racket/base

(require (only-in "exceptions.rkt"
                  exn:fail:filesystem:errno:file-not-found?
                  refine-filesystem-exn)
         "try.rkt"
         html-parsing
         net/url
         racket/bool
         racket/contract/base
         racket/contract/region
         racket/function
         racket/match
         racket/port
         racket/system)

(provide (all-from-out "exceptions.rkt")
         urlish/c                    ; path, string, or url struct
         is-local?         ; given a url-ish thing, determine if it's a local resource
         to-url            ; convert to a url
         url-as-string     ; convert a urlish/c into a string 
         ->absolute-url    ; generate an absolute url from base and component
         web/call          ; get a page from the internet with a lot of processing options
         fetch-with-curl   ; shell out to curl in order to fetch HTTPS pages on OSX (ARGH!)
         get-page          ; more readable but less flexible wrapper around web/call
         )

(define urlish/c (or/c path-string? url?))

;;----------------------------------------------------------------------
;;    Check if a url-ish thing refers to a local resource or one on
;;    the net.
(define/contract (is-local? s)
  (-> urlish/c boolean?)
  (define scheme (url-scheme (to-url s)))
  (or (false? scheme)
      (equal? "file" scheme)
      (equal? 'file  scheme)))

;;----------------------------------------------------------------------
;;    Turn a url-ish thing (string, path, url) into an url
;;
;; #:treat-string-as-path? means that if it's a string that does not
;; specify a URL scheme then it should be given the 'file' scheme.  We
;; do this by round-tripping through url->path and path->url so that
;; the local OS's filesystem preferences will be respected.
;;
(define/contract (to-url val #:treat-string-as-path? [string-is-path? #f])
  (->* (urlish/c) (#:treat-string-as-path? boolean?)  url?)

  (define converted
    ((match val
       [(? url?)    identity]
       [(? string?) string->url]
       [_           path->url])
     val))

  (cond [(url-scheme converted) converted]   ; if it has a scheme, change nothing
        [(and (string? val) string-is-path?) (path->url (url->path converted))]
        [else converted]))

;;----------------------------------------------------------------------

(define url-as-string (compose1 url->string to-url))

;;----------------------------------------------------------------------
;;    Two args, 'b' and 'u'.  Both can be path, string, or url.  If
;;    'u' is absolute, return it.  If it is relative, combine it with
;;    'b'.  In either case, the return value is a url structure
;;    regardless of how it came in.
(define/contract (->absolute-url b u)
  (-> urlish/c urlish/c url?)
  (define u (to-url u))
  (if (url-scheme u)
      u
      (combine-url/relative (to-url b)
                            (url->string u))))

;;----------------------------------------------------------------------
;;    Get a page from the internet. Accepts a path, string, or url.
;;    'call-proc' gets passed to call/input-url.  'post-proc' gets the
;;    result of call/input-url.  Returns the result of post-proc.  By
;;    default returns an xexp representing the page.
;;
(define/contract (web/call url-string
                           #:call-proc    [call-proc port->string]
                           #:connect-proc [conn-proc (curry get-pure-port #:redirections 5)]
                           #:post-proc    [post-proc html->xexp]
                           #:as-text      [as-text #f]) ;; Really just a convenient shortcut
  (->* (urlish/c)
       (#:call-proc (-> input-port? any)
        #:post-proc (-> any/c any)
        #:as-text boolean?)
       any)
  ((if as-text identity post-proc)
   (call/input-url (to-url url-string)
                   conn-proc
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
;;    file. 'post-proc' (default: html->xexp) will be run across the
;;    results.
;;
;;    If you're getting from a filepath (either as a path or a string)
;;    and the file doesn't exist then an
;;    exn:fail:filesystem:errno:file-not-found will be raised. (This
;;    exception is defined in handy/exceptions.rkt) If you'd rather
;;    that it simply return #f, pass #:must-exist? #f.  (Example:
;;    You're checking an on-disk cache and only going to the web if
;;    it's not in the cache)
(define/contract (get-page source
                           #:source-string-as-path? [string-as-path? #f]
                           #:post-proc [post-proc html->xexp]
                           #:as-text   [as-text? #f] ; same as '#:post-proc identity'
                           #:must-exist? [must-exist? #t]
                           ) 
  (->* (urlish/c)
       (#:source-string-as-path? boolean?
        #:post-proc (-> string? any/c)
        #:as-text boolean?
        #:must-exist? boolean?
        )
       any)
  (define target-url (to-url source #:treat-string-as-path? string-as-path?))
  (define content
    (cond [(not (is-local? target-url)) (web/call target-url #:as-text #t)]
          [else
           (try [(with-input-from-file
                   (url->path target-url)
                   (thunk (port->string)))]
                [catch (exn:fail:filesystem?
                        (lambda (e)
                          (define refined (refine-filesystem-exn e))
                          (cond [must-exist? (raise refined)]
                                [(exn:fail:filesystem:errno:file-not-found? refined) #f]
                                [else (raise refined)])))])]))

  (cond [(or as-text? (false? content)) content]
        [else (post-proc content)]))

