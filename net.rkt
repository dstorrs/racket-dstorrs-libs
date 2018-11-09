#lang racket/base

(require net/url
         racket/contract
         racket/format
         racket/function
         racket/string
         )

(provide url-string?
         url-string->string
         url-string->url
         build-url
         )

;;----------------------------------------------------------------------

(define/contract (url-string? thing)
  (-> any/c boolean?)
  (or (string? thing) (url? thing)))

;;----------------------------------------------------------------------

;; *) url-string->string and url-string->url : convert an (url or string) to a (string or url)
(define/contract (url-string->string x)
  (-> url-string? string?)
  (if (string? x) x (url->string x)))

;;----------------------------------------------------------------------

(define/contract (url-string->url x)
  (-> url-string? url?)
  (if (string? x) (string->url x) x))

;; ;;----------------------------------------------------------------------

(define/contract (build-url addr targ
                            #:as-string? [as-string? #f]
                            #:use-https? [use-https? #t])
  (->* (url-string? (or/c  url-string? list?))
       (#:as-string? boolean? #:use-https? boolean?)
       url-string?)
  (define address (regexp-replace #px"^::1" (url-string->string addr) "localhost"))
  (define target  (url-string->string
                   (if (url-string? targ)
                       targ
                       (string-join (map ~a targ) "/"))))
  
  (define scheme  (if use-https? "https:/" "http:/"))
  
  ;; Examples:
  ;;
  ;;    (build-url "google.com:25679" "foobar")
  ;;    (build-url "google.com:25679" '("foobar" 7)
  ;;
  ;;    Results:
  ;;        (string->url "http://google.com:25679/foobar")
  ;;        (string->url "http://google.com:25679/foobar/7")
  ;;
  ;;    If you set the #:as-string? optional parameter to #t then you'll get it
  ;;    back as a string, not a url struct.
  (define u (string-join (list scheme address target) "/"))
  (if as-string? u (string->url u)))
