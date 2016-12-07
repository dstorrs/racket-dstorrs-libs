#lang racket

;;    Functions:
;; *) firstn, restn : first and rest, but they return '() when given '()
;; *) atom? : true if something is not a pair. (symbol, number, vector...)
;; *) autobox : ensure that its argument is a list. If not, returns (list arg)
;; *) remove-nulls : filter '()s out of a list
;; *) list-remf* filter all desired elements out of a list, by default #<void>
;; *) list/not-null? : is it a pair and not '()? NB: checks for pair,
;;     not list, so it treats '(x . y) as a list
;; *) step-by-n : repeatedly call a function on next N elements of a list
;; *) member-rec : finds matching elements in sublist as well as main list
;; *) vector->dict, list->dict : turn a vector/list into some kind of
;;     dict (typically a hash)
;; *) in-range-inc : inclusive ranges
;; *) find-contiguous-ranges : search a list for contiguous segments,
;;     return a list of sublists
(define (firstn l) (if (null? l) '() (first l)))
(define (restn  l) (if (null? l) '() (rest l)))

(define (atom? x) (not (pair? x)))

(define (autobox x) (if (list? x) x (list x)))

(define (remove-nulls l) (filter (negate null?) l))

;;    Create list with conditional elements
;; (list 'a 'b (when x 'c) 'd)        => either '(a b c d) or '(a b #<void> d)
;; (list-remf* 'a 'b (when x 'c) 'd)  => either '(a b c d) or '(a b d)
;; (list-remf* 'a "b" #:pred string?) => '(a)
(define (list-remf* #:pred [pred void?] . l)
  (remf* pred l))

(define (list/not-null? l) (and (not (atom? l)) (not (null? l))))

(define/contract (step-by-n func data [num 2])
  (-> procedure? list? list?)
  (if (null? data)
      '()
	  (append (autobox (apply func (take data num)))
			  (let ((l (drop data num)))
				(step-by-n func
                           l
						   num)))))

;;----------------------------------------------------------------------
;; NOTE: This is obsoleted by #lang rackjure.  Prefer that.
;;
;;    Take a data structure built of nested (hashes, lists, vectors)
;;    and retrieve items from it.  Hashes are accessed by key, vectors
;;    and lists by index. If the struct is neither a hash nor a list,
;;    it just returns the struct.  Examples:
;;
;;  (define h (hash "foo" '(a b c) "bar" 8 "quux" (vector "d" "e" "f")))
;;  (get h     '("foo" 1))   -> 'b
;;  (get h     '("bar"))     -> 8
;;  (get h     '("quux" 1))  -> "e"
;;  (get "bob" '("apple"))   -> "bob"
;;
;;    The optional def argument allows you to specify a default.  The
;;    default is returned iff one of the following exceptions is
;;    thrown:
;;        #(struct:exn:fail:contract list-ref: index too large for list
;;        #(struct:exn:fail:contract hash-ref: no value found for key
;;        #(struct:exn:fail:contract vector-ref: index is out of range
;;
(define (get s keys [def #f])
  (define (get-once key s)
	(cond
      [(hash? s)   (hash-ref   s key)]
      [(list? s)   (list-ref   s key)]
      [(vector? s) (vector-ref s key)]
      [else s]))
  (with-handlers
    ((exn:fail:contract?
      (lambda (e)
        (cond
          ((not (regexp-match #px"(no value found for key|index (too large|out of range))"
                              (exn-message e)))
           (raise e))
          ((false? def) (raise e))
          (else def)))))
    (foldl get-once s (autobox keys))))

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

(define/contract (vector->dict keys data #:dict-maker [dict-maker make-hash] #:transform [transform identity])
  (->* (list? (or/c #f vector?))  ;; keys and data
       (#:dict-maker procedure?
        #:transform  (-> dict? dict?) ;; transform the resulting dict before returning
        )
       dict?)
  (cond ((not data) (dict-maker));; Makes handling DB queries easier
        ((not (= (length keys) (vector-length data)))
         (raise "In vector->dict, data (vector) and keys (list) must be the same length"))
        (else (list->dict keys (vector->list data) #:dict-maker dict-maker #:transform transform))))

;;----------------------------------------------------------------------

(define (in-range-inc x [y #f])
  (stream->list (if y (in-range x (add1 y)) (in-range (add1 x)))))

;;----------------------------------------------------------------------

(define (list->assoc-list lst)
  (step-by-n (compose list cons) lst))

;;----------------------------------------------------------------------

(define/contract (list-of-2->pair l)
  (-> (list/c any/c any/c) pair?)
  (cons (first l) (second l)))

;;----------------------------------------------------------------------

(define/contract (list->dict keys
                             data
                             #:dict-maker [dict-maker make-hash]
                             #:transform [transform-post identity]
                             )
  (->* (list? list?)         ;; keys and data
       (#:dict-maker (-> (listof pair?) dict?) ; takes an assoc list, returns a dict
        #:transform (-> dict? dict?)           ; transform the output of dict-maker
        )
       dict?)

  (unless (= (length data) (length keys))
    (raise "In list->dict, data and keys must be the same length"))

  (transform-post (dict-maker (map (compose list-of-2->pair list) keys data)))
  )

;;----------------------------------------------------------------------

;;    Generate a list of lists where each sublist is a sequence of
;;    consecutive chunk-nums.  For example, if the hashes in 'data'
;;    had these chunk nums:
;;
;;        '(1 2 3 5 7 200 201 202 203))
;;
;;    Then you would get this result:
;;
;;        '((2 3) (5) (7) (200 201 202 203))
;;

(define/contract (find-contiguous-runs data #:key  [extract-key identity])
  (->* (list?) (#:key (-> any/c exact-integer?)) list?)
  (define result '())
  (define-values (n final)
    (for/fold ((prev (car data))
               (acc  (list (car data)))
               )
              ((curr (cdr data)))
      (values curr
              (if (= (extract-key curr) (add1 (extract-key prev)))
                  (cons curr acc)
                  (begin
                    (set! result (cons (reverse acc) result))
                    (list curr))))
      ))
  (reverse (cons (reverse final) result))
  )

;;----------------------------------------------------------------------

(provide (all-defined-out))
