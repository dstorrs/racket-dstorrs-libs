#!/usr/bin/env racket

#lang racket

(require "../test-more.rkt"
         "../try.rkt")


(when #t
  (test-suite
   "defatalize"

   (lives (thunk
           (is (defatalize (raise "foo"))
               "foo"
               "defatalize returns the exception"))
          "defatalized a (raise <string>")

   (lives (thunk
           (define e (defatalize (raise-argument-error 'foo "bar" 8)))
           (is-type e exn:fail? "got an exception object as expected"))
          "defatalized a (raise-argument-error)")
   )
  )

(done-testing)
