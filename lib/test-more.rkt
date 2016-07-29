#lang racket

(require (for-syntax syntax/parse)
		 racket/stxparam
		 racket
		 racket/splicing
		 )

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

(define-syntax (like stx)
  (syntax-parse stx
				[(like val regex)
				 #'(ok (regexp-match regex val))]
				[(like val regex msg)
				 #'(ok (regexp-match regex val) msg)]))

(define-syntax (unlike stx)
  (syntax-parse stx
				[(unlike val regex)
				 #'(test-more-check #:expr val
									#:op (lambda (a b) (false? a))
									#:report-expected/got #f
									)]
				[(unlike val regex msg)
				 #'(test-more-check #:expr val
									#:op (lambda (a b) (false? a))
									#:msg msg
									#:report-expected/got #f
									)]))



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


;;   (define (build-predicate pred)
;; 	(let ([m (lambda (e) (exn-message e))])
;; 	  (cond
;; 	   [(string? pred) (lambda (e) (equal? pred (m e)))]
;; 	   [(regexp? pred) (lambda (e) (regexp-match pred (m e)))]
;; 	   [(procedure? pred) pred])))
;;   (begin
;; 	(println "main body, thunk:")
;; 	(println thunk)
;; 	(with-handlers ([exn?
;; 					 (test-more-check
;; 					  #:expr ((lambda (e)
;; 							   (begin
;; 							   (println "in handler")
;; 							   (let ([p (build-predicate pred)])
;; 								 (println (format "~a~a" "p is:" p))
;; 								 #t))))
;; 					  #:msg msg)])
;; 				   (thunk))))

;;   (define (build-predicate pred)
;; 	(cond
;; 	 [(procedure? pred) pred]
;; 	 [(string?    pred)
;; 	  (lambda (exn)
;; 		;;    Snip the boilerplate off the exception message
;; 		(let* ([str (exn-message exn)]
;; 			   [str (regexp-replace #px"^.+?expected: " str "")]
;; 			   [str (regexp-replace #px"(.+)\n.+$" str "\\1")])
;; 		  (equal? str pred)))]
;; 	 [(regexp? pred)
;; 	  (lambda (exn) (regexp-match pred (exn-message exn)))]))

;; 	(with-handlers
;; 	 ([((build-predicate pred) exn)
;; 	   (test-more-check    #:expr #t  #:msg msg)]
;; 	  [#t (test-more-check #:expr #f  #:msg msg)])
;; 	 (expr)))


(provide ok not-ok is isnt test-more-check like unlike throws)

