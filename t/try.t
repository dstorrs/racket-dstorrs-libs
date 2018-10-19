#!/usr/bin/env racket

#lang racket/base

(require racket/function
         "../test-more.rkt"
         "../try.rkt")

(expect-n-tests 22)

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

   (let ((pre-clause-executed #f)
         (catch-clause-executed #f))
     (define val (try [pre (set! pre-clause-executed #t) 8]
                      [2]))
     (ok (and pre-clause-executed
              (equal? val 2))
         "pre/try is a legal syntax and it properly distinguished the clauses"))

   (let ((pre-clause-executed #f)
         (catch-clause-executed #f))
     (try [pre (set! pre-clause-executed #t)]
          [(raise "foo")]
          [catch (identity (lambda (e) (set! catch-clause-executed #t)))])
     (ok (and pre-clause-executed
              catch-clause-executed)
         "pre/try/catch is a legal syntax"))

   (let ((pre-clause-executed #f)
         (catch-clause-executed #f)
         (finally-clause-executed #f))
     (try [pre (set! pre-clause-executed #t)]
          [(raise "foo")]
          [catch (identity (lambda (e) (set! catch-clause-executed #t)))]
          [finally (set! finally-clause-executed #t)])
     (ok (and pre-clause-executed
              catch-clause-executed
              finally-clause-executed)
         "pre/try/catch/finally is a legal syntax"))

   (let ((finally-clause-executed #f)
         (pre-clause-executed #f))
     (try [(raise "foo")]
          [pre (set! pre-clause-executed #t)]
          [catch (identity (lambda (e) #t))]
          [finally (set! finally-clause-executed 8)])
     (ok #t "body/pre/catch/finally is legal syntax")
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

   (let ((finally-clause-executed #f))
     (try [#t]
          [finally (set! finally-clause-executed 8)])
     (ok finally-clause-executed "try+finally is a legal form"))

   (let ([finally-clause-executed #f]
         [pre-clause-executed #f])
     (try [#t]
          [pre (set! pre-clause-executed #t)]
          [finally (set! finally-clause-executed 8)])
     (ok pre-clause-executed "try+pre+finally is a legal form, and the pre executed")
     (ok finally-clause-executed "try+pre+finally is a legal form, and the finally executed"))
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
