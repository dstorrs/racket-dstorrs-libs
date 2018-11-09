#!/usr/bin/env racket

#lang at-exp racket/base

(require net/url
         racket/format
         "../test-more.rkt"
         "../net.rkt")

(expect-n-tests 7)

(when #t
  (test-suite
   "build-url"
   
   (is (build-url "google.com" "list-apis")
       (string->url "https://google.com/list-apis")
       @~a{(build-url "google.com" "list-apis") works})

   (is (build-url "google.com" '("list-apis" 7))
       (string->url "https://google.com/list-apis/7")
       @~a{(build-url "google.com" '("list-apis" 7)) works})


   (is (build-url "google.com" '("list-apis" 7) #:use-https? #f)
       (string->url "http://google.com/list-apis/7")
       @~a{(build-url "google.com" '("list-apis" 7) #:use-https? #f) works})


   (is (build-url "google.com" "list-apis" #:as-string? #t)
       "https://google.com/list-apis"
       @~a{(build-url "google.com" "list-apis" #:as-string? #t) works})

   (is (build-url "google.com" '("list-apis" 7) #:as-string? #t)
       "https://google.com/list-apis/7"
       @~a{(build-url "google.com" '("list-apis" 7) #:as-string? #t) works})

   (is (build-url "google.com" '("list-apis" 7) #:as-string? #t #:use-https? #f)
       "http://google.com/list-apis/7"
       @~a{(build-url "google.com" '("list-apis" 7) #:as-string? #t #:use-https? #f) works})
   ))
