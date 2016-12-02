#lang racket

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
			  (define (next-test-num)
				(set! test-num (add1 test-num))
				test-num))

(define (_unwrap-val val) (if (procedure? val) (val) val))

(define (test-more-check  #:expr     ex
						  #:expected [expected #t]
						  #:msg      [m ""]
						  #:op       [op equal?]
						  #:report-expected/got [ex-report #t]
						  )
  (let* ([res ex]
         [success (op res expected)]
		 [ok-str (if success "ok " "NOT ok ")]
		 [msg (format "~a~a"
					  (if (non-empty-string? m)
						  (format " - ~a" m)
						  "")
					  (if (or res (false? ex-report))
						  ""
						  (format "\n\tExpected: ~a\n\tGot:      ~a"
								  expected
								  res)))])
    (define result-func (if success tests-passed tests-failed))
    (result-func 1)
	(displayln (format "~a~a~a"
					   ok-str
					   (next-test-num)
					   msg
					   ))
    success)
  )

(define (ok val [msg ""])
  (test-more-check #:expr (_unwrap-val val)
				   #:msg msg
				   #:report-expected/got #f
				   #:op (lambda (a b) (not (false? a)))
				   ))

(define (not-ok val [msg ""])
  (ok (false? (_unwrap-val val))
	  msg))


;;    (is-type (my-func) hash? "(my-func) returns a hash")
(define (is-type val type-pred [msg ""] [op equal?])
  (test-more-check #:expr (type-pred val)
				   #:msg msg
				   #:op op
				   ))

(define (is val expected [msg ""] [op equal?])
  (test-more-check #:expr (_unwrap-val val)
				   #:expected expected
				   #:msg msg
				   #:op op
				   ))

(define (isnt val
			  expected
			  [msg ""]
			  [op (lambda (a b) (not (equal? a b)))])
  (is val expected msg op))

(define (like val regex [msg ""])
  (ok (regexp-match regex val) msg))

(define/contract (unlike val regex [msg ""])
  (->* (any/c regexp?)
	   (string?)
	   any/c)
  (ok (not (regexp-match regex val)) msg))


(define/contract (lives thunk [msg ""])
  (->* (procedure?) (string?) any/c)
  (with-handlers ((exn? (lambda (e)
						  (test-more-check #:expr #f
										   #:msg (format "Exception thrown! Test message: '~a'.  Exception: '~a'" msg (exn-message e))))))
				 (begin
				   (thunk)
				   (test-more-check #:expr #t  #:msg msg))))


;; note that if you give it a function predicat that predicate must
;; take one argument but it can be anything, not just an (exn?)
(define/contract (throws thunk pred [msg ""])
  (->* (procedure? (or/c string? regexp? (-> any/c boolean?))) 
	   (string?)
	   any/c)
  ;;    'thunk' should generate an exception
  ;;    'msg'  is what test-more-check will report
  ;;    'pred' could be a string, a proc, or a regex
  ;;        - string: Check if it is the (non-boilerplate) exn message
  ;;        - proc:   Pass it the exn, see if it returns #t
  ;;        - regex:  Check if the regex matches the exn message
  ;;
  (define (remove-exn-boilerplate s)
	(let* ([str (regexp-replace #px"^.+?expected: " s "")]
		   [str (regexp-replace #px"(.+)\n.+$" str "\\1")])
	  str))

  (test-more-check #:expr
				   (with-handlers
					([exn?
					  (lambda (e)
						(let ((msg (exn-message e)))
						  (cond
						   ((string? pred) (equal? pred (remove-exn-boilerplate e)))
						   ((regexp? pred) (regexp-match? pred msg))
						   ((procedure? pred) (pred e))
						   (else #f)
						   )))]
					 [exn? (lambda (e) #f)])
					(thunk))
				   #:msg msg))
(define/contract (dies thunk pred [msg ""])
  (->* (procedure? (or/c string? regexp? (-> any/c boolean?)))
	   (string?)
	   any/c)
  (throws thunk pred msg))

(define-syntax (test-suite stx)
  (syntax-case stx ()
	[(_ msg body body1 ...)
	 #'(begin (say "### START test-suite: " msg)
              (void (lives (thunk body body1 ...)
					 "test-suite completed without throwing exception"))
              (say "")
              (say "Total tests passed so far: " (tests-passed))
              (say "Total tests failed so far: " (tests-failed))
              (say "")
			  (say "### END test-suite: " msg))]))


(provide ok not-ok
		 is isnt
         is-type
		 test-more-check
		 like unlike
		 throws dies lives
		 test-suite)

