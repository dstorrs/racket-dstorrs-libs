#lang racket/base

(require (for-syntax racket/base
                     syntax/parse)
         racket/function)

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
;;    Standard with-handlers.  (NB:  functions in the body are just examples, not real)
;; (with-handlers ([exn:fail:contract?   (lambda (e) ...lots of stuff...)]
;;                 [string?              (lambda (e) ...more stuff...)]
;;                 [exn:fail:tcp-connect? (lambda (e) ...even more stuff...)])
;;
;;   (define server-handle (or (connect-to-server) (raise "could not connect")))
;;   (define user (get-user-data server-handle (get-password)))
;;   ...etc...
;;  )
;;
;;  ;With the above code you're four lines in before you find out what
;;  ;the code actually does...and that's assuming that the exception
;;  ;handlers are concise, one-line things, which they might not be.
;;  ;Compare to an equivalent try/catch:
;;
;; (try [
;;        (define server-handle (or (connect-to-server) (raise "could not connect")))
;;        (define user (get-user-data server-handle (get-password)))
;;        ...etc...
;;      ]
;;      [catch ([exn:fail:contract?   (lambda (e) ...lots of stuff...)]
;;              [string?              (lambda (e) ...more stuff...)]
;;              [exn:fail:tcp-connect? (lambda (e) ...even more stuff...)])])
;;
;;   ; With the try/catch block you immediately know that you're
;;   ; dealing with something that's connecting to a server and fetching
;;   ; user data.  You can read the happy path first to understand
;;   ; what's supposed to happen, then look in the 'catch' block to see
;;   ; how it handles problems.
;;
;;
;;   ; But wait, there's more!  Want to have some cleanup that is
;;   ; guaranteed to happen even if there's an exception?  We've got you
;;   ; covered
;; (try [(displayln "foo") (raise-syntax-error  'name "message")]
;;      [catch (exn:fail? (lambda (e) (displayln "caught!")))]
;;      [finally (displayln "finally 1")  (displayln "finally 2")]
;;      )
;;
;; Output:
;; foo
;; finally 1
;; finally 2
;; caught!
;;
;;    ; Still not enough?  How about some preflight setup that is
;;    ; guaranteed to happen before the body executes, even in the
;;    ; presence of jumping in and out via continuation?
;; (try [(displayln "body") (raise "foo")]
;;      [pre (displayln "pre")]
;;      [catch (string? (lambda (e) (displayln "caught!")))]
;;      [finally (say "finally")])
;;
;;  Output:
;;  pre
;;  body
;;  finally!
;;  caught!
;;
;;    ; 'try' still works inside a dynamic-wind even though it generates one:
;; (dynamic-wind
;;   (thunk (displayln "outer pre"))
;;   (thunk (try [(displayln "body") (raise "error!")]
;;               [pre (displayln "body pre")]
;;               [catch (string? (lambda (e) (displayln "body catch")))]
;;               [finally (displayln "body finally")]))
;;   (thunk (displayln "outer finally")))
;;
;; Output:
;; outer pre
;; body pre
;; body
;; body finally
;; body catch
;; outer finally
;;
;;    The return value from a try is:
;; *) If no exception raised:  The result of the 'try' block
;; *) If exception raised:     The result of the 'catch' block
;;
;;    The following combinations are legal:
;; try    ; traps anything that's raised, returns it.  (i.e., defatalizes it)
;; try + catch
;; try + pre + catch
;; try + catch + finally
;; try + pre + catch + finally

(define-syntax (try stx)
  (syntax-parse stx
    #:datum-literals (try catch pre finally)
    [(try [body0:expr body1:expr ...])
     #'(with-handlers ((identity identity))
         body0
         body1
         ...
         )]
    [(try [body0:expr body1:expr ...]
          [catch catch0:expr catch1:expr ...])
     #'(with-handlers (catch0 catch1 ...)
         body0
         body1
         ...
         )]
    [(try [pre p0:expr p1:expr ... ]
          [body0:expr body1:expr ...])
     #'(with-handlers ((identity identity))
         (dynamic-wind
           (thunk p0 p1 ...)
           (thunk body0
                  body1
                  ...)
           (thunk '()))
         )]
    [(try [pre p0:expr p1:expr ... ]
          [body0:expr body1:expr ...]
          [catch catch0 catch1 ...])
     #'(with-handlers (catch0 catch1 ...)
         (dynamic-wind
           (thunk p0 p1 ...)
           (thunk body0
                  body1
                  ...)
           (thunk '()))
         )]
    [(try [pre p0:expr p1:expr ... ]
          [body0:expr body1:expr ...]
          [finally f0 f1 ...])
     #'(with-handlers ((identity identity))
         (dynamic-wind
           (thunk p0 p1 ...)
           (thunk body0
                  body1
                  ...)
           (thunk f0 f1 ...))
         )]
    [(try [pre p0:expr p1:expr ... ]
          [body0:expr body1:expr ...]
          [catch catch0 catch1 ...]
          [finally f0 f1 ...])
     #'(with-handlers (catch0 catch1 ...)
         (dynamic-wind
           (thunk p0 p1 ...)
           (thunk body0
                  body1
                  ...)
           (thunk f0 f1 ...))
         )]
    [(try [body0 body1 ...]
          [catch catch0 catch1 ...]
          [finally f0 f1 ... ])
     #'(with-handlers (catch0 catch1 ...)
         (dynamic-wind
           (thunk '())
           (thunk body0
                  body1
                  ...)
           (thunk f0 f1 ...)
           ))]
    [(try [body0 body1 ...]
          [finally f0 f1 ... ])
     #'(with-handlers ([(lambda (x) #t) raise])
         (dynamic-wind
           (thunk '())
           (thunk body0
                  body1
                  ...)
           (thunk f0 f1 ...)
           ))]
    [(try [body0 body1 ...]
          [pre p0 p1 ... ]
          [finally f0 f1 ... ])
     #'(with-handlers ([(lambda (x) #t) raise])
         (dynamic-wind
           (thunk p0 p1 ...)
           (thunk body0
                  body1
                  ...)
           (thunk f0 f1 ...)
           ))]
    [(try [body0 body1 ...]
          [pre pre0 pre1 ...]
          [catch catch0 catch1 ... ])
     #'(with-handlers (catch0 catch1 ...)
         (dynamic-wind
           (thunk pre0 pre1   ...)
           (thunk body0 body1 ...)
           (thunk '())
           ))]
    [(try [body0 body1 ...]
          [pre     pre0 pre1 ...]
          [catch   catch0 catch1 ...]
          [finally f0 f1 ... ])
     #'(with-handlers (catch0 catch1 ...)
         (dynamic-wind
           (thunk pre0 pre1 ...)
           (thunk body0
                  body1
                  ...)
           (thunk f0 f1 ...)
           ))]
    ))

(define-syntax (defatalize stx)
  (syntax-parse stx
    [(defatalize body0 body1 ...)
     #'(with-handlers ([exn:break? raise]
                       [(lambda (e) #t) identity])
         body0
         body1 ...)]))

(provide  (all-defined-out))
