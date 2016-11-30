#lang racket

(require racket/syntax)

;;  PURPOSE: Racket's with-handlers has poor end weight; it puts the
;;  error conditions first, distracting from the actual point of the
;;  code.  try/catch is a better mechanism -- the code comes first
;;  with the error handling tucked out of the way where you don't need
;;  to look at it unless you actually need to look at it.
;;  try/catch/finally also provides an easy way to do cleanup during
;;  exception handling.
;
;;  USAGE:
;;
;;    Standard with-handlers
;; (with-handlers ([(or exn? string?) (lambda (e) (say "boom"))])
;;   (raise-argument-error 'foo "foo" "foo"))
;;
;; Output:
;; boom!
;;
;;    Basic try/catch:
;; (try [(say "foo") (raise-syntax-error "foo")]
;;      [catch (exn:fail? (lambda (e) (displayln "guarded!")))]
;;      )
;;
;; Output:
;; foo
;; guarded!
;;
;;    Full try/catch/finally:
;; (try [(say "foo") (raise-syntax-error "foo")]
;;      [catch (exn:fail? (lambda (e) (displayln "guarded!")))]
;;      [finally (displayln "finally 1")  (displayln "finally 2")]
;;      )
;;
;; Output:
;; foo
;; finally 1
;; finally 2
;; guarded!
;;
;;    Still works inside a dynamic-wind even though it generates one:
;; (dynamic-wind
;;   (thunk (displayln "outer pre"))
;;   (thunk (try [(displayln "body") (raise "error!")]
;;               [catch (string? (lambda (e) (displayln "body guard")))]
;;               [finally (displayln "body finally")]))
;;   (thunk (displayln "outer post")))
;;
;; Output:
;; outer pre
;; body
;; body finally
;; body guard
;; outer post

(define-syntax (try stx)
  (syntax-case stx ()
    [(try [body0 body1 ...][catch catch0 catch1 ...])
     #'(with-handlers (catch0 catch1 ...)
         body0
         body1
         ...
         )]
    [(try [body0 body1 ...][catch catch0 catch1 ...][finally f0 f1 ... ])
     #'(with-handlers (catch0 catch1 ...)
         (dynamic-wind
           (thunk '())
           (thunk body0
                  body1
                  ...)
           (thunk f0 f1 ...)
         ))]
    [(try [body0 body1 ...]
          [pre p0 p1 ...]          
          [catch catch0 catch1 ...]
          [finally f0 f1 ... ])
     #'(with-handlers (catch0 catch1 ...)
         (dynamic-wind
           (thunk p0 p1 ...)
           (thunk body0
                  body1
                  ...)
           (thunk f0 f1 ...)
         ))]
    ))

(provide  (all-defined-out))
