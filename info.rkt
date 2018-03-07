#lang info

(define collection "handy")
(define version "1.0")
(define test-omit-paths '("test-more.rkt"))

(define deps '("html-parsing"
               "base"
               "db-lib"
               "rackunit-lib"
               "sxml"
               ))
(define build-deps '("at-exp-lib"
                     ))
