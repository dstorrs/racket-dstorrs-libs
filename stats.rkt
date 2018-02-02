#lang racket

(require racket/contract
		 math
		 "./list-utils.rkt"
		 )

(provide (all-defined-out))


;;    How many standard deviations is 'score' from mean?
(define (zscore score mean stddev)
  (-> number? number? number? number?)
  (/ (- score mean) stddev))

;; (define (correlation-by-zscore l m)
;;   (-> (listof number?) (listof number?) number?)
;;   ;;  (raise exn:fail:user "correlation-by-zscore not implemented"))
;;   (let ((bad-arg (lambda () (raise exn:fail:contract "arguments must be equal-length non-null lists")))
;; 		(is-non-null-list? (lambda (l) (or (atom? l)))

;;   (when ((or (atom? l
;; 	(cond
;; 	 (
;; 	 ((null? l) (bad-arg))
;; 	 ((null? m) (bad-arg))
;; 	  ((null? m) #f)
;; 	  ((not (= (length l) (length m))) #f)
;; 	  (else
;; 	   (let*
;; 		   (
;; 			(zscore-l (curryr zscore (mean l) (std-dev l)))
;; 			(zscore-m (curryr zscore (mean m) (std-dev m)))
;; 			(row (lambda (x y) (* (zscore-l x) (zscore-m y))))
;; 			)
;; 		 (/ (sum (map row l m)) (- (length l) 1))))
;; 	  ))

(define/contract (tau-a l m)
  ;; Take two lists of rankings, compare them looking for concordant
  ;; and discordant pairs where 'concordant' means "# of pairs below
  ;; this that are larger than this one".
  ;; Calculation is C-D / (n * (n-1))/2
  ;;    l and m must  be equal-length lists of exact integers
  (->i ([list1 (listof (and/c exact-integer? (>/c 0)))]
		[list2 (list1) (and/c
						(listof (and/c exact-integer? (>/c 0)))
						(lambda (list2) (= (length list1)
										   (length list2))))])
	   [result real?])
  (raise exn:fail:user "tau-a not implemented"))
