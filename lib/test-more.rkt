#lang racket

(require racket racket/splicing)

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
		 [ok-str (if (op res expected) "ok " "NOT ok ")]
		 [msg (format "~a~a"
					  (if (non-empty-string? m)
						  (format " - ~a" m)
						  "")
					  (if (or res (false? ex-report))
						  ""
						  (format "\n\tExpected: ~a\n\tGot:      ~a"
								  expected
								  res)))])
	(displayln (format "~a~a~a"
					   ok-str
					   (next-test-num)
					   msg
					   ))))

(define (ok val [msg ""])
  (test-more-check #:expr (if (procedure? val) (val) val)
				   #:msg msg
				   #:report-expected/got #f
				   #:op (lambda (a b) (not (false? a)))
				   ))

(define (not-ok val [msg ""])
  (ok (if (procedure? val) (not (val)) (not val))
	  msg))


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
						(let ((s (remove-exn-boilerplate (exn-message e))))
						  (cond
						   ((string? pred) (equal? pred s))
						   ((regexp? pred) (regexp-match? pred s))
						   ((procedure? pred) (pred e))
						   (else #f)
						   )))]
					 [exn? (lambda (e) #f)])
					(thunk))
				   #:msg msg))



(provide ok not-ok is isnt test-more-check like unlike throws)

