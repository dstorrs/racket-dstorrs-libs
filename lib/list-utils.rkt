#lang racket

(define (atom? x) (not (pair? x)))

(define (autobox x) (if (list? x) x (list x)))

(define (remove-nulls l) (filter (negate null?) l))

(define (list-not-null? l) (and (not (atom? l)) (not (null? l))))

(define (step-by-n func data [num 2])
  (if (null? data)
	  (func '())
	  (append (autobox (func (take data num)))
			  (let ((l (drop data num)))
				(step-by-n func
						   (if (> (length l) num) l (error "wrong number of args"))
						   num)))))

;;----------------------------------------------------------------------
;;    Take a data structure built of nested hashes and lists, retrieve
;;    items from it.  Hashes are accessed by key, lists by index. If
;;    the struct is neither a hash nor a list, it just returns the
;;    struct.  Examples:
;;
;;  (define h (hash "foo" '(a b c) "bar" 8))
;;  (get h     '("foo" 1))   -> 'b
;;  (get h     '("bar"))     -> 8
;;  (get "bob" '("foo" 3))   -> "bob"
;;
(define (get s keys) 
  (define (data/ref s key)
	(cond
	 [(hash? s) (hash-ref s key)]
	 [(list? s) (list-ref s key)]
	 [else s]))
  (define (get-once key s) (data/ref s key))
  (foldl get-once s (autobox keys)))

;;----------------------------------------------------------------------

;;    Search through a list for all instances of an item.  e.g.:
;; (member-rec 
;; (define (member-rec a)
;;   (cond
;;    ((atom? a) null)
;;    ((null? a) null)
;;    ((and (list? a) (equal? (car a) 'table))
;; 	(append (list a)
;; 			(member-rec (car a))
;; 			(member-rec (cdr a))))
;;    (else (append (member-rec (car a))
;; 				 (member-rec (cdr a))))))

(define (member-rec match lst)
  (cond
   ((atom? lst) null)
   ((null? lst) null)
   ((and (list? lst) (equal? (car lst) 'table))
	(append (list lst)
			(member-rec match (car lst))
			(member-rec match (cdr lst))))
   (else (append (member-rec match (car lst))
				 (member-rec match (cdr lst))))))
;;----------------------------------------------------------------------
(provide (all-defined-out))
