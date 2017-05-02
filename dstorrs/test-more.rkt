#lang racket

;;    TODO:
;; - Make it show got and expected when test fails
;; - Add 'disable this test suite' keyword
;; - Add 'TODO this test suite' keyword
;; - Add 'too few tests ran' detection
;; - Fix the issue where it sometimes shows 'expected #f got #f'
;; - On test-suite, add 'setup' and 'cleanup' keywords that take thunks

(require racket
         racket/splicing
         dstorrs/utils
         )

(define tp 0)
(define tf 0)

(define (tests-passed [inc 0])
  (set! tp (+ tp inc))
  tp)

(define (tests-failed [inc 0])
  (set! tf (+ tf inc))
  tf)

(splicing-let ([test-num 0])
  (define (_inc-test-num! inc)
    (set! test-num (+ test-num inc))
    )
  (define (next-test-num)
    (set! test-num (add1 test-num))
    test-num))

(define (_unwrap-val val) (if (procedure? val) (val) val))

;;----------------------------------------------------------------------

(define/contract
  (test-more-check  #:got           got
                    #:expected      [expected #t]
                    #:msg           [msg ""]
                    #:op            [op equal?]
                    #:show-expected/got? [show-expected/got? #t]
                    #:report-expected-as [report-expected-as #f]
                    #:report-got-as      [report-got-as #f]
                    #:return             [return #f]
                    )
  (->* (#:got any/c)
       (#:expected any/c
        #:msg string?
        #:op (-> any/c any/c any/c)
        #:show-expected/got? boolean?
        #:report-expected-as any/c
        #:report-got-as any/c
        #:return any/c
        )
       any)
  (let* ([success (op got expected)]
         [ok-str (if success "ok " "NOT ok ")]
         [expected-msg (~a (or report-expected-as expected))]
         [got-msg (~a (or report-got-as got))]
         [msg-str (format "~a~a"
                          (if (non-empty-string? msg)
                              (format " - ~a" msg)
                              "")
                          (if (and (not success) show-expected/got?)
                              (format "\n  Got:      ~a\n  Expected: ~a"
                                      got-msg
                                      expected-msg
                                      )
                              ""
                              ))])
    (define pass/fail-counter (if success tests-passed tests-failed))
    (pass/fail-counter 1)
    (displayln (format "~a~a~a"
                       ok-str
                       (next-test-num)
                       msg-str
                       ))
    (if return 
        return ; if we were told what to return, return that
        got))  ; otherwise, return the result
  )

;;----------------------------------------------------------------------

(define (ok val [msg ""])
  (test-more-check #:got (_unwrap-val val)
                   #:msg msg
                   #:show-expected/got? #f
                   #:op (lambda (a b) (not (false? a)))
                   ))

(define (not-ok val [msg ""])
  (ok (false? (_unwrap-val val))
      msg))


;;    (is-type (my-func) hash? "(my-func) returns a hash")
(define (is-type val type-pred [msg ""] [op equal?])
  (test-more-check #:got (type-pred val)
                   #:msg msg
                   #:op op
                   #:return val
                   ))

(define (is val expected [msg ""] [op equal?])
  (test-more-check #:got (_unwrap-val val)
                   #:expected expected
                   #:msg msg
                   #:op op
                   #:return val
                   ))

(define (isnt val
              expected
              [msg ""]
              [op (lambda (a b) (not (equal? a b)))])
  (test-more-check #:got (_unwrap-val val)
                   #:expected expected
                   #:msg msg
                   #:report-expected-as (~a "<anything but " expected ">")
                   #:op (negate equal?)))

;;----------------------------------------------------------------------

(define (like val regex [msg ""])
  (define res (regexp-match regex (_unwrap-val val)))
  (test-more-check #:got (true? res) ; force to boolean
                   #:return res
                   #:msg msg
                   #:report-expected-as (~a "<something matching " regex ">")))

;;----------------------------------------------------------------------

(define/contract (unlike val regex [msg ""])
  (->* (any/c regexp?)
       (string?)
       any/c)
  (test-more-check #:got (_unwrap-val val)
                   #:expected #t
                   #:msg msg
                   #:report-expected-as (~a "<something NOT matching " regex ">")
                   #:op (lambda (a b) (not (regexp-match regex val)))))



(define/contract (lives thunk [msg ""])
  (->* (procedure?) (string?) any/c)
  (with-handlers ((exn? (lambda (e)
                          (test-more-check #:got #f
                                           #:msg (format "Exception thrown! Test message: '~a'.  Exception: '~a'" msg (exn-message e))))))
    (define result (thunk))
    (test-more-check #:got result  #:expected result  #:msg msg)))

;;----------------------------------------------------------------------

;; note that if you give it a function predicate that predicate must
;; take one argument but it can be anything, not just an (exn?)
(define/contract (throws thnk pred [msg ""])
  (->* ((-> any)
        any/c
        )
       (string?)
       any/c)

  ;;    'thnk' should generate an exception
  ;;    'msg'  is what test-more-check will report
  ;;    'pred' could be a string, a proc, or a regex
  ;;        - string: Check if it is exactly the (non-boilerplate) exn message
  ;;        - proc:   Pass it the exn, see if it returns #t
  ;;        - regex:  Check if the regex matches the (exn message || string) thrown
  ;;
  (define (get-msg e) (if (exn? e) (exn-message e) e))
  (define (remove-exn-boilerplate s)
    (let* ([str (regexp-replace #px"^.+?expected: " (get-msg s) "")]
           [str (regexp-replace #px"(.+)\n.+$" str "\\1")])
      str))

  (define (accept-all e) #t)
  (define-values (e threw)
    (with-handlers ((exn:break? (lambda (e) (raise e))) ; if user hit ^C, don't eat it
                    (accept-all (lambda (e) (values e #t))))
      (values (thnk) #f)))
    
  (define pred-needs-string (or (string? pred) (regexp? pred)))
  (define e-can-be-string   (or (string? e) (exn? e)))
  (when (and pred-needs-string (not e-can-be-string))
    (raise-arguments-error 'throws
                           "predicate was (string or regexp) but thrown value was not (string or exn)"
                           "thrown value" e))

  (cond [(false? threw)    (test-more-check #:got #f  #:msg (~a msg " [DID NOT THROW]")  #:return e)]
        [(procedure? pred) (test-more-check #:msg msg #:got (pred e) #:report-got-as e #:return e)]
        [(string? pred)    (test-more-check #:msg msg #:got (equal? pred (remove-exn-boilerplate e)) #:report-got-as e  #:return e)]
        [(regexp? pred)    (test-more-check #:msg msg #:got (regexp-match? pred (get-msg e)) #:report-got-as e  #:return e)]
        [else              (test-more-check #:msg msg #:got e  #:expected pred #:return e)]
        )
  )

;;----------------------------------------------------------------------

;;    When all you care about is that it dies, not why
(define/contract (dies thunk [msg ""])
  (->* (procedure?)
       (string?)
       any/c)
  (throws thunk (lambda (e) #t) msg))

;;----------------------------------------------------------------------

(define-syntax (test-suite stx)
  (syntax-case stx ()
    [(_ msg body body1 ...)
     #'(begin (say "### START test-suite: " msg)
              (void (lives (thunk body body1 ...)
                           "test-suite completed without throwing (uncaught) exception"))
              (say "")
              (say "Total tests passed so far: " (tests-passed))
              (say "Total tests failed so far: " (tests-failed))
              (say "")
              (say "### END test-suite: " msg))]))

;;----------------------------------------------------------------------

(define/contract (make-test-file fpath [text (rand-val "test file contents")] #:overwrite [overwrite #t])
  (->* (path-string?) (string? #:overwrite boolean?) path-string?)
  (define-values (dir fn ignore) (split-path fpath))

  (when (not (directory-exists? dir))
    (make-directory dir))
  
  (define filepath
    (cond [(file-exists? fpath) fpath]
          [(directory-exists? fpath) (build-path fpath (rand-val "test-file"))]
          [else fpath]))
  
  (with-output-to-file
    filepath
    (thunk (display text))
    #:exists (if overwrite 'replace 'error))
  filepath)

;;----------------------------------------------------------------------

(provide ok not-ok
         is isnt
         is-type
         test-more-check
         like unlike
         throws dies lives
         test-suite
         tests-failed tests-passed
         _inc-test-num!
         make-test-file
         )
