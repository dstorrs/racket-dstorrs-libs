#lang racket

;;    Functions:
;; *) alist->hash alist : turn an alist like '((a . 1) (b . 2)) into an immutable hash
;; *) atom? : true if something is not a pair. (symbol, number, vector...)
;; *) autobox : ensure that its argument is a list. If not, returns (list arg)
;; *) disjunction : find the elements of one dict that are not in the other
;; *) in-range-inc : inclusive ranges
;; *) find-contiguous-runs : search a list for contiguous segments,
;;     return a list of sublists
;; *) hash->keywork-apply : take a function and a hash.  Assume the
;;     keys of the hash are keyword arguments and call appropriately.
;; *) L : alias for 'list'
;; *) list-remf* filter all desired elements out of a list, by default #<void>
;; *) list/not-null? : is it a pair and not '()? NB: checks for pair,
;;     not list, so it treats '(x . y) as a list
;; *) member-rec : finds matching elements in sublist as well as main list
;; *) multi-partition : like the standard partition, but accepts multiple destination lists
;; *) remove-nulls : filter '()s out of a list
;; *) safe-first, safe-rest : first and rest, but they return '() when given '()
;; *) sort-num, sort-str, sort-sym : shorthand for (sort) with number<?, string<?, or symbol<?
;; *) sort-smart : call sort-num, sort-str, or sort-sym depending on first element of list
;; *) step-by-n : repeatedly call a function on next N elements of a list
;; *) symbols->keywords  : takes a list of symbols, returns a sorted list of keywords
;; *) unique : return a list of the unique non-null elements in a list, in the order seen
;; *) vector->dict, list->dict : turn a vector/list into some kind of
;;     dict (by default a mutable hash)

(define L list)
(define/contract (safe-first l [default '()])
  (->* (list?) (any/c) any/c)
  (cond [(null? l) default]
        [else (first l)]))

(define/contract (safe-rest  l [default '()])
  (->* (list?) (any/c) any/c)
  (cond [(null? l) default]
        [else (rest l)]))


(define (atom? x) (not (pair? x)))

(define (autobox x) (if (list? x) x (list x)))

(struct dict-disjunction (different only-in-first only-in-second dict-first dict-second) #:transparent)
(define/contract (disjunction dict1 dict2)
  (-> hash? hash? dict-disjunction?)

  (define diff (make-hash))
  (define first-only (make-hash))
  (define second-only (make-hash))

  (for ((k (remove-duplicates (append (hash-keys dict1) (hash-keys dict2)))))
    (cond [(not (hash-has-key? dict1 k))
           (hash-set! second-only k (hash-ref dict2 k))]

          [(not (hash-has-key? dict2 k))
           (hash-set! first-only k (hash-ref dict1 k))]

          [(not (equal? (hash-ref dict1 k) (hash-ref dict2 k)))
           (hash-set! diff k (list (hash-ref dict1 k) (hash-ref dict2 k)))]
          ))
  (dict-disjunction diff first-only second-only dict1 dict2))

(define (remove-nulls l) (filter (negate null?) l))

;;    Create list with conditional elements
;; (list 'a 'b (when x 'c) 'd)        => either '(a b c d) or '(a b #<void> d)
;; (list-remf* 'a 'b (when x 'c) 'd)  => either '(a b c d) or '(a b d)
;; (list-remf* 'a "b" #:pred string?) => '(a)
(define (list-remf* #:pred [pred void?] . l)
  (remf* pred l))

(define (list/not-null? l) (and (not (atom? l)) (not (null? l))))

;;----------------------------------------------------------------------

(define/contract (sort-num lst) (-> (listof number?) list?)  (sort lst <))
(define/contract (sort-str lst) (-> (listof string?) list?)  (sort lst string<?))
(define/contract (sort-sym lst) (-> (listof symbol?) list?)  (sort lst symbol<?))
(define/contract (sort-smart lst)
  (-> (listof any/c) list?)
  (cond [(null? lst) '()]
        [(number? (first lst)) (sort-num lst)]
        [(string? (first lst)) (sort-str lst)]
        [(symbol? (first lst)) (sort-sym lst)]
        [else (raise-arguments-error 'sort-smart
                                     "all elements of list must of same type (number, string, or symbol)"
                                     "args list" lst)]))

;;----------------------------------------------------------------------

(define/contract (step-by-n func data [step-size 2] #:return-results [return-results #t])
  (->* ((unconstrained-domain-> any/c) sequence?) (exact-positive-integer? #:return-results boolean?) (or/c void? list?))

  ;; if data is empty, return null
  ;; if data is shorter than the step size, use whatever remains and then return
  ;; if data is >= step size, process step-size elements and recur
  ;;
  ;; func can take any number of arguments.  It will be executed via
  ;; (apply func <step-data>)
  ;;
  ;; step-by-n returns the result as a list of lists where each
  ;; sublist is the results of one particular step.  For example:
  ;;
  ;;    -> (define h '(1 2 3 4 5 6 7))
  ;;    -> (step-by-n + h 2)
  ;;    '((3) (7) (11) (7))
  ;;    -> (step-by-n list h 2)
  ;;    '((1 2) (3 4) (5 6) (7))
  ;;
  ;; If you are iterating for side effects (e.g. inserting into a DB)
  ;; and traversing an enormous source then you can choose to discard
  ;; the results by setting #:return-results #f in order to avoid
  ;; making a massive list in memory.

  (cond [return-results 
         (for/list ((next-group (in-slice step-size data)))
           (apply func next-group))]
        [else 
         (for ((next-group (in-slice step-size data)))
           (apply func next-group))]))

;;----------------------------------------------------------------------
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
;;    The default value cannot be #f because that means 'do not return
;;    a default'.
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

(define/contract (in-range-inc x [y #f])
  (->* (exact-integer?) (exact-integer?) stream?)
  (if y (in-range x (add1 y)) (in-range (add1 x))))

;;----------------------------------------------------------------------

(define (list->assoc-list lst)
  (step-by-n (compose list cons) lst))

;;----------------------------------------------------------------------

(define/contract (vector->dict keys
                               data
                               #:dict-maker [dict-maker make-hash]
                               #:transform-dict [transform-dict identity]
                               #:transform-data [transform-data cons]
                               )
  (->* (list? vector?)                           ; keys and data
       (#:dict-maker (-> (listof pair?) dict?)   ; takes an assoc list, returns a dict
        #:transform-dict (-> dict? dict?)        ; transform the output of dict-maker
        #:transform-data (-> any/c any/c pair?)  ; transform the input of dict-maker
        )
       dict?)

  (if (not data)
      (dict-maker);; Makes handling DB queries easier
      (list->dict keys
                  (vector->list data)
                  #:dict-maker dict-maker
                  #:transform-dict transform-dict
                  #:transform-data transform-data
                  )))

;;----------------------------------------------------------------------

(define/contract (list->dict raw-keys
                             data
                             #:dict-maker [dict-maker make-hash]
                             #:transform-dict [transform-dict identity]
                             #:transform-data [transform-data cons]
                             #:make-keys  (key-maker #f)
                             )
  (->* (list? list?)         ;; keys and data
       (#:dict-maker     (-> (listof pair?) dict?) ; takes an assoc list, returns a dict
        #:transform-data (-> any/c any/c pair?)    ; transform the input of dict-maker
        #:transform-dict (-> dict? dict?)          ; transform the output of dict-maker
        #:make-keys      (-> any/c any/c)          ; generate the keys list based on data
        )
       dict?)

  (let ((keys (if key-maker (map key-maker data) raw-keys)))
    (unless (= (length data) (length keys))
      (raise-arguments-error 'list->dict
                             (string-append "data and "
                                            (if key-maker
                                                "(map <make-keys-func> data)"
                                                "keys")
                                            " must be the same length")
                             "keys" (if key-maker keys raw-keys)
                             "data" data))

    (transform-dict (dict-maker (map transform-data keys data)))
    )
  )

;;----------------------------------------------------------------------

;;    Generate a list of lists where each sublist is a sequence of
;;    consecutive chunk-nums.  For example, if given this list:
;;
;;        '(1 2 3 5 7 200 201 202 203))
;;
;;    Then you would get this result:
;;
;;        '((1 2 3) (5) (7) (200 201 202 203))
;;

(define/contract (find-contiguous-runs data #:key  [extract-key identity])
  (->* (list?) (#:key (-> any/c exact-integer?)) list?)
  (if (null? data)
      null
      (let () ; need the let only to establish a definition context
        (define-values (n final result)
          (for/fold ((prev (car data))
                     (acc  (list (car data)))
                     (result '())
                     )
                    ((curr (cdr data)))

            ;; if curr is contiguous with prev, add curr to acc (build the run)
            ;; if curr not contiguous, add acc to result and clear acc for next time

            (define is-contiguous (= (extract-key curr) (add1 (extract-key prev))))
            (values curr                  ;; the one we just processed becomes 'prev(ious)'
                    (if is-contiguous     ;; add to or clear accumulator
                        (cons curr acc)
                        (list curr))
                    (if is-contiguous     ;; add accumulator to result if the run is over
                        result
                        (cons (reverse acc) result)
                        ))
            ))
        (reverse (cons (reverse final) result)))
      ))

;;----------------------------------------------------------------------

(define/contract (flatten/convert func lst)
  (-> procedure? list? list?)
  (flatten (map func lst)))

;;----------------------------------------------------------------------

(define/contract (alist->hash alist)
  (-> (listof pair?) (and/c hash? immutable?))
  (apply hash (flatten alist)))

;;----------------------------------------------------------------------

(define/contract (unique lst [same? equal?] #:key [key-maker identity])
  (->* (list?) ((-> any/c any/c boolean?) #:key (-> any/c any/c)) list?)
  (remove-nulls
   (remove-duplicates lst same? #:key key-maker)))

;;----------------------------------------------------------------------

(define/contract (symbols->keywords lst)
  (-> (listof symbol?) (listof keyword?))
  (map (compose string->keyword symbol->string) (sort lst symbol<?)))

;;----------------------------------------------------------------------

(define/contract (multi-partition #:partitions num-dests
                                  #:filter index-chooser
                                  #:source source
                                  #:post-process-partition [post-process-partition identity]
                                  #:post-process-element   [post-process-element   (lambda (idx elem) elem)]
                                  )
  (->* (#:partitions exact-positive-integer?
        #:filter (-> any/c (or/c #f void? exact-nonnegative-integer?))
        #:source list?)
       (
        #:post-process-partition (-> list? any/c)
        #:post-process-element   (-> exact-nonnegative-integer? any/c any/c)
        )
       any
       )
  (cond [(= 1 num-dests) source]
        [else
         (define max-idx (sub1 num-dests))
         (define results (make-vector num-dests '()))

         (for ((element (reverse source)))
           (let ((idx (index-chooser element)))
             (cond [(or (false? idx)
                        (void? idx)) 'do-nothing]
                   [(> idx max-idx)
                    (raise-result-error 'multi-partition
                                        "index between 0 and one less than number of partitions"
                                        idx)]
                   [else (vector-set! results
                                      idx
                                      (cons (post-process-element idx element)
                                            (vector-ref results idx)))])))
         (for ((i (vector-length results)))
           (vector-set! results
                        i
                        (post-process-partition
                         (vector-ref results i))))

         (vector->values results)]))

;;----------------------------------------------------------------------

(define/contract (hash->keyword-apply func hsh [positionals '()])
  (->* (procedure? (hash/c symbol? any/c)) (list?) any)

  (define keys (sort (hash-keys hsh) symbol<?))

  (keyword-apply func
                 (symbols->keywords keys)
                 (map (curry hash-ref hsh) keys)
                 positionals))

;;----------------------------------------------------------------------

(provide (all-defined-out)
         (struct-out dict-disjunction))
