#lang racket

(define (atom? x) (not (pair? x)))

(define (autobox x) (if (list? x) x (list x)))

(define (remove-nulls l) (filter (negate null?) l))

(define (list-not-null? l) (and (not (atom? l)) (not (null? l))))

(define (step-by-n func data [num 2])
  (if (null? data)
	  (func '())
	  (append (autobox (func (take data num)))
			  (step-by-n func (drop data num) num))))

;;----------------------------------------------------------------------
;;    Take a data structure built of nested hashes and lists, retrieve
;;    items from it.  Hashes are accessed by key, lists by index. If
;;    the struct is neither a hash nor a list, it just returns the
;;    struct.  Examples:
;;
;;  (define h (hash "foo" '(a b c) "bar" 8))
;;  (-> h     '("foo" 1))   -> 'b
;;  (-> h     '("bar"))     -> 8
;;  (-> "bob" '("foo" 3))   -> "bob"
;;
(define (-> s keys) 
  (define (data/ref s key)
	(cond
	 [(hash? s) (hash-ref s key)]
	 [(list? s) (list-ref s key)]
	 [else s]))
  (define (fetch-once key s) (data/ref s key))
  (foldl fetch-once s keys))

;;----------------------------------------------------------------------
(provide (all-defined-out))
