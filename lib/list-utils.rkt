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
;;    Search through a list recursively for all instances of an item,
;;    includes ones that are nested inside other instances.  The item
;;    can be either a value or a predicate.  Returns a list of all
;;    instances; nested items will appear both in their parent and on
;;    their own.  e.g.:
;;
;; (define l '(1 2 (table 1) ((4) 5 (((table 2 (table 3)))))))
;; (member-rec 2 l)                  => '(2 2)
;; (member-rec (curry equal? 2) l)   => '(2 2)
;; (member-rec number? l)            => '(1 2 1 4 5 2 3)
;; (member-rec (lambda (x) (and
;; 						 (list? x)
;; 						 (not (null? x))
;; 						 (equal? (car x) 'table)))
;; 			l)  => '((table 1) (table 2 (table 3)) (table 3))
;;
(define/contract (member-rec m lst)
  (-> any/c any/c list?)
  (define match (if (procedure? m) m (curry equal? m)))
  (define search (compose autobox (curry member-rec match)))
  (define (recur l) (append (search (car l)) (search (cdr l))))
  (cond
   ((atom? lst) (if (match lst) (list lst) null))
   ((null? lst) null)
   ((match lst) (append (list lst) (recur lst)))
   (else (recur lst))))

;;----------------------------------------------------------------------
(provide (all-defined-out))
