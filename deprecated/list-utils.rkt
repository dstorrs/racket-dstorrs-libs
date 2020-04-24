#lang racket/base

(require racket/require
         (multi-in racket (contract function)))

(provide alist->hash
         L
         member-rec
         )


;;----------------------------------------------------------------------

;;  NOT DEPRECATED.  Redefined here from the main module so that I can
;;  use them without having a circular dependency.
(define/contract (autobox x)
  (-> any/c list?)
  (if (list? x) x (list x)))

(define atom? (negate pair?))


;;----------------------------------------------------------------------
;;----------------------------------------------------------------------
;;----------------------------------------------------------------------

; DEPRECATED FROM HERE DOWN

(define alist->hash make-immutable-hash)

;;----------------------------------------------------------------------
;;
;; I don't know what I was thinking when I wrote this.  It's stupid.
;;
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
  (-> any/c list? list?)
  (define match (if (procedure? m) m (curry equal? m)))
  (define search (compose autobox (curry member-rec match)))
  (define (recur l) (append (search (car l)) (search (cdr l))))
  (cond
    ((atom? lst) (if (match lst) (list lst) null))
    ((null? lst) null)
    ((match lst) (append (list lst) (recur lst)))
    (else (recur lst))))

;;----------------------------------------------------------------------

(define L list)

