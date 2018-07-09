#lang info

(define collection "handy")
(define version "2.8")
(define test-omit-paths '("test-more.rkt"))

(define deps '("html-parsing"
               "base"
               "db-lib"
               "rackunit-lib"
               "sxml"
               ))
(define build-deps '("at-exp-lib"
                     ))
