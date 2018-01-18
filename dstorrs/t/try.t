#!/usr/bin/env racket

#lang racket

(require "../test-more.rkt"
         "../try.rkt")


(when #t
  (test-suite
   "try"

   (is (try [(+ 2 2) (+ 1 1)][catch (identity (lambda (e) (raise e)))])
       2
       "try works when the main body doesn't throw; returns final value")
   (throws (thunk (try [(+ 1 1) (raise "foo")]
                       [catch (string? (lambda (e) (raise "expected this")))]))
           #px"expected this"
           "exceptions are caught as expected")
   (let ([finally-clause-executed #f]
         [catch-clause-executed #f])
     (try [#t]
          [catch (identity (lambda (e) (set! catch-clause-executed #t) (raise e)))]
          [finally (set! finally-clause-executed 7)])
     (ok (and finally-clause-executed
              (not catch-clause-executed))
         "'finally' clause executes if you exit without error"))

   (let ((finally-clause-executed #f))
     (try [(raise "foo")]
          [catch (identity (lambda (e) #t))]
          [finally (set! finally-clause-executed 8)])
     (ok finally-clause-executed "'finally' clause executes if you have an exception"))

   (let ((pre-clause-executed #f)
         (catch-clause-executed #f))
     (try [(raise "foo")]
          [pre (set! pre-clause-executed #t)]
          [catch (identity (lambda (e) (set! catch-clause-executed #t)))])
     (ok (and pre-clause-executed
              catch-clause-executed)
         "try/pre/catch is a legal syntax"))

   (let ((finally-clause-executed #f)
         (pre-clause-executed #f))
     (try [(raise "foo")]
          [pre (set! pre-clause-executed #t)]
          [catch (identity (lambda (e) #t))]
          [finally (set! finally-clause-executed 8)])
     (ok finally-clause-executed "'finally' clause executes if you have an exception")
     (ok pre-clause-executed "'pre' clause executes if you have an exception"))

   (let ((finally-clause-executed #f)
         (pre-clause-executed #f))
     (try [#t]
          [pre (set! pre-clause-executed #t)]
          [catch (identity (lambda (e) #t))]
          [finally (set! finally-clause-executed 8)])
     (ok finally-clause-executed "'finally' clause executes if you have no exception")
     (ok pre-clause-executed "'pre' clause executes if you have no exception"))
   )
  )

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
