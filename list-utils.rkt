#lang racket

(require (only-in handy/hash hash->keyword-apply))
(provide (all-defined-out)
         (all-from-out handy/hash)
         (struct-out dict-disjunction))

;;    Parameters:
;; *) current-dict-maker-function : customize what vector->dict and list->dict do
;; *) current-transform-data-function
;; *) current-transform-data-function
;; *) make-transform-data-func

;;    Functions:
;; *) alist->hash alist : turn an alist like '((a . 1) (b . 2)) into an immutable hash
;; *) atom? : true if something is not a pair. (symbol, number, vector...)
;; *) autobox : ensure that its argument is a list. If not, returns (list arg)
;; *) compose-fifo : like 'compose' but applies args from left to right, not right to left
;; *) disjunction : find the elements of one dict that are not in the other
;; *) in-range-inc : inclusive ranges
;; *) find-contiguous-runs : search a list for contiguous segments,
;;     return a list of sublists
;; *) L : alias for 'list'
;; *) list->values  ; because it's dumb that this doesn't exist
;; *) list-remf* filter all desired elements out of a list, by default #<void>
;; *) list/not-null? : is it a pair and not '()? NB: checks for pair,
;;     not list, so it treats '(x . y) as a list
;; *) member-rec : finds matching elements in sublist as well as main list
;; *) multi-partition : like the standard partition, but accepts multiple destination lists
;; *) remove-nulls : filter '()s out of a list
;; *) safe-first, safe-rest : first and rest, but they return specified value when given '()
;; *) slice :  return elements of a list from index x to index y, or as much as remains
;; *) sort-num, sort-str, sort-sym : shorthand for (sort) with number<?, string<?, or symbol<?
;; *) sort-smart : call sort-num, sort-str, or sort-sym depending on first element of list
;; *) step-by-n : repeatedly call a function on next N elements of a list
;; *) symbols->keywords  : takes a list of symbols, returns a sorted list of keywords
;; *) unique : return a list of the unique non-null elements in a list, in the order seen
;; *) unwrap-list : when given a list of one list, return the inner
;;     list.  Useful when dealing with rest params
;; *) vector->dict, list->dict : turn a vector/list into some kind of
;;     dict (by default a mutable hash)

(define current-dict-maker-function     (make-parameter make-hash))
(define current-transform-data-function (make-parameter cons))
(define current-transform-dict-function (make-parameter identity))

;;----------------------------------------------------------------------

;;    (make-transform-data-func pg-array? pg-array->list sql-null? #f)
;;
;; The above line returns a func that will take a key and a value,
;; convert values that are pg-arrays into lists and values that are
;; sql-null into #f, then return (cons key val).  You can parameterize
;; it into current-transform-data-function for all your data-cleaning
;; needs.  Also, see the derived parameter in db.rkt:
;; current-query-as-dict-transform-data-function
;;
(define/contract (make-transform-data-func . args)
  (->* () () #:rest (and/c (listof any/c) (compose even? length)) procedure?)

  (define the-map
    (make-immutable-hash
     (step-by-n (lambda (p f)
                  (define pred (if (procedure? p) p (curry equal? p)))
                  (define func (if (procedure? f) f (lambda (ignore) f)))
                  (cons pred func))
                args)))

  (lambda (key val)
    (call/ec (lambda (return)
               ;;  Check to see if our value matches one of the
               ;;  predicates / key-vals in our hash
               (for ([(pred func) the-map])
                 (when (pred val)
                   (return (cons key (func val)))))
               ;; Nope, it didn't.
               (cons key val)))))


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

;;----------------------------------------------------------------------

(define/contract (compose-fifo . args)
  (->* () () #:rest (listof procedure?) any)
  (define funcs (if (null? args) values args))
  (apply compose (reverse funcs)))

;;----------------------------------------------------------------------

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

;;----------------------------------------------------------------------

(define/contract (list->values l)
  (-> list? any)
  (vector->values (list->vector l)))

;;----------------------------------------------------------------------

;;    Create list with conditional elements
;; (list 'a 'b (when x 'c) 'd)        => either '(a b c d) or '(a b #<void> d)
;; (list-remf* 'a 'b (when x 'c) 'd)  => either '(a b c d) or '(a b d)
;; (list-remf* 'a "b" #:pred string?) => '(a)
(define (list-remf* #:pred [pred void?] . l)
  (remf* pred l))

(define (list/not-null? l) (and (not (atom? l)) (not (null? l))))

;;----------------------------------------------------------------------

(define/contract (remove-duplicates/rec lst
                                        #:key [extract-key identity])
  (->* (list?)
       (#:key  (-> any/c any/c))
       list?)

  (define (_helper lst
                   [same? equal?]
                   #:key [extract-key identity]
                   #:seen [seen (hash)])

    (for/fold ([result '()]
               [already-seen seen])
              ([item lst])

      (cond [(list? item)
             (define-values (res new-seen)
               (_helper item
                        same?
                        #:key extract-key
                        #:seen already-seen))
             (values (cons res result) new-seen)]
            ;
            [(hash-ref already-seen (extract-key item) #f)
             (values result already-seen)]
            ;
            [else
             (values (cons item result)
                     (hash-set already-seen (extract-key item) #t))])))

  (define-values (res ignore) (_helper lst
                                       ;same? ; @@TODO  PUT THIS IN LATER
                                       #:key extract-key))

  (reverse res))


;;----------------------------------------------------------------------

(define/contract (slice lst start [end #f])
  (->* (list? exact-nonnegative-integer?)
       (exact-nonnegative-integer?)
       list?)

  ; (slice lst 2 5) => indices 2,3,4,5

  (cond [(null? lst) '()]
        [else
         (define lst-length (length lst))
         (define last-idx   (sub1 lst-length))

         (cond [(not end)          (drop lst start)] ; no end specified
               [(> start last-idx) '()]              ; invalid start
               [(> end   last-idx) (drop lst start)] ; invalid end
               [else  (take (drop lst start)
                            (add1 (- end start)))])]))

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

(define/contract (step-by-n func data [step-size 2] #:return-results? [return-results? #t] #:pass-args-as-list? [pass-args-as-list? #f])
  (->* ((unconstrained-domain-> any/c) sequence?) (exact-positive-integer? #:return-results? boolean? #:pass-args-as-list? boolean?) (or/c void? list?))

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
  ;; #:return-results?  If you are iterating for side effects
  ;; (e.g. inserting into a DB) and traversing an enormous source then
  ;; you can choose to discard the results by setting #:return-results?
  ;; #f in order to avoid making a massive list in memory.
  ;;
  ;; #:pass-args-as-list?  If you would like your function to receive
  ;; its arguments as a list instead of as separate values, set this
  ;; to #t.  This is useful if you're going to be iterating in very
  ;; large steps.
  (cond [return-results?
         (for/list ((next-group (in-slice step-size data)))
           (cond [pass-args-as-list? (func next-group)]
                 [else (apply func next-group)]))]
        [else
         (for ((next-group (in-slice step-size data)))
           (cond [pass-args-as-list? (func next-group)]
                 [else (apply func next-group)]))]))

;;----------------------------------------------------------------------
;;
;;    Take a data structure built of nested (hashes, lists, vectors,
;;    structs) and retrieve items from it.  Hashes are accessed by
;;    key, vectors and lists by index, and structs by function. If
;;    the data is not a recognized thing then get returns the data.
;;    Examples:
;;
;;  (define h (hash "foo" '(a b c) "bar" 8 "quux" (vector "d" "e" "f")))
;;  (get h     '("foo" 1))   -> 'b
;;  (get h     '("bar"))     -> 8
;;  (get h     '("quux" 1))  -> "e"
;;  (get "bob" '("apple"))   -> "bob", since "bob" isn't a recognized type
;;
;;    As a special case, if there is only one key then you can pass it
;;    directly instead of in a list:
;;
;;  (get h     "bar")        -> 8
;;
;;    The optional def argument allows you to specify a default.  The
;;    default is returned iff one of the following exceptions is
;;    thrown:
;;        #(struct:exn:fail:contract list-ref: index too large for list
;;        #(struct:exn:fail:contract hash-ref: no value found for key
;;        #(struct:exn:fail:contract vector-ref: index is out of range
;;
;;  (get h  '("quux" 77) 'not-found)  ; returns 'not-found
;;
;;    Note that you can pass functions as 'keys' even when the thing
;;    you'll be accessing is not a struct.  For example:
;;
;;  (get h  (list "bar" add1)) ; returns 9
;;
(define/contract (get s keys [def 'no-default-value-specified])
  (->* (any/c any/c)
       (any/c)
       any/c)

  (define (get-once key s)
    (cond
      [(hash? s)         (hash-ref   s key)]
      [(list? s)         (list-ref   s key)]
      [(vector? s)       (vector-ref s key)]
      [(procedure? key)  (key s)]
      [else              s]))

  (define list-of-keys (autobox keys))

  (cond [(null? list-of-keys) s]
        [else
         (with-handlers
           ([exn:fail:contract?
             (lambda (e)
               (cond
                 [(equal? def 'no-default-value-specified) (raise e)]
                 [(not (regexp-match #px"(no value found for key|index (too large|out of range))"
                                     (exn-message e)))
                  (raise e)]
                 [else def]))])
           (foldl get-once s (autobox keys)))]))

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
                               #:dict-maker [dict-maker (current-dict-maker-function)]
                               #:transform-dict [transform-dict (current-transform-dict-function)]
                               #:transform-data [transform-data (current-transform-data-function)]
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

;; list->dict
;;
;; Takes a list of keys and a list of data, as well as some optional
;; keyword params.  Default functionality is to cons the keys to their
;; respective data then send that to make-hash in order to generate a
;; mutable hash.  Through application of the various optional params
;; you can make it generate anything.  NOTE: The contract does not
;; actually require that the return value be a dict; if you know what
;; you're doing and want to generate something else, that's fine.
;;
;; NB:  A dict is either:
;;    - a hash
;;    - a vector (uses exact integers as keys)
;;    - a list of pairs  (could be either cons pairs or proper non-null lists of any length)
;;    - structures that implement the gen:dict interface
;;
;; See the contract for what the various parameter must be.
;;
;; #:transform-data     Default: cons.       Accepts a key and a value, returns a pair.
;; #:dict-maker         Default: make-hash.  Gets the list of pairs from transform-data
;; #:transform-dict     Default: identity.   Operate on the result of dict-maker
;; #:make-keys          Default: #f          If set, generates the keys based on the data
;;
;;    ***NOTE*** dict-maker's default value is actually whatever is in
;;    the current-dict-maker-function parameter.  By default that's
;;    make-hash.
;;
;; Examples:
;;
;;    (list->dict '(foo bar) '(7 8))
;;        => (make-hash '((foo . 7) (bar . 8)))
;;    (list->dict '(foo bar) '(7 8) #:transform-data (lambda (k v) (cons k (add1 v)))
;;        => (make-hash '((foo . 8) (bar . 9)))
;;    (list->dict '(foo bar) '(7 8) #:transform-dict (curryr hash-set! 'baz 9))
;;        => (make-hash '((foo . 7) (bar . 8) (baz . 9)))
;;    (list->dict '(foo bar) '(7 8) #:dict-maker make-immutable-hash
;;        => (make-immutable-hash '((foo . 7) (bar . 8)))
;;    (list->dict '(foo bar) '(65 66) #:make-keys integer->char   ; NB: specified keys were ignored
;;        => (make-hash '((#\A . 65) (#\B . 66)))
;;    (list->dict '() '(65 66) #:make-keys integer->char
;;        => (make-hash '((#\A . 65) (#\B . 66)))
(define/contract (list->dict raw-keys
                             data
                             #:dict-maker [dict-maker (current-dict-maker-function)]
                             #:transform-dict [transform-dict (current-transform-dict-function)]
                             #:transform-data [transform-data (current-transform-data-function)]
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

;;(define/contract (find-contiguous-runs data #:key  [extract-key identity])
;;
;; Generate a list of lists where each sublist is a sequence of
;; consecutive elements.  For example, if given this list:
;;
;;    '(1 2 3 5 7 200 201 202 203))
;;
;; Then you would get this result:
;;
;;    '((1 2 3) (5) (7) (200 201 202 203))
;;
;; Operates only on exact integers, but you can provide a function
;; that will generate a number from your data.  For example:
;;
;;     (find-contiguous-runs (list (hash 'age 17) (hash 'age 18) (hash 'age 27))
;;                           #:key (curryr hash-ref 'age))
;;
;;     Returns:  (list (list (hash 'age 17) (hash 'age 18))
;;                     (list (hash 'age 27)))
;;
(define/contract (find-contiguous-runs data
                                       #:key [extract-key identity]
                                       #:op  [op #f])
  (->* (list?)
       (#:key (-> any/c any/c)
        #:op  (-> any/c any/c boolean?)
        )
       list?)

  (define contiguous? (or op
                          (lambda (a b) (= (add1 (extract-key a)) (extract-key b)))))

  (cond [(null? data) '()]
        [else
         (define-values (extract-key-a extract-key-b)
           (cond [(list? extract-key) (list->values extract-key)]
                 [else (values extract-key extract-key)]))

         (define-values (n final result)
           (for/fold ((prev (car data))
                      (acc  (list (car data)))
                      (result '())
                      )
                     ((curr (cdr data)))

             ;; if curr is contiguous with prev, add curr to acc (build the run)
             ;; if curr not contiguous, add acc to result and clear acc for next time

             (define is-contiguous (contiguous? prev curr))
             (values curr                  ;; the one we just processed becomes 'prev(ious)'
                     (if is-contiguous     ;; add to or clear accumulator
                         (cons curr acc)
                         (list curr))
                     (if is-contiguous     ;; add accumulator to result if the run is over
                         result
                         (cons (reverse acc) result)
                         ))
             ))
         (reverse (cons (reverse final) result))]))

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

(define/contract (unwrap-list lst)
  (-> list? list?)
  (cond [(null? lst) lst]
        [(> (length lst) 1) lst]
        [(list? (car lst)) (car lst)]
        [else lst]))

;;----------------------------------------------------------------------

(define/contract (symbols->keywords lst)
  (-> (listof symbol?) (listof keyword?))
  (map (compose string->keyword symbol->string) (sort lst symbol<?)))

;;----------------------------------------------------------------------

;    Example:
;
;  (multi-partition #:partitions 3 #:source '(1 2 3 4 5 6 7) #:filter (curryr modulo 3))
;     Returns: (values '(3 6) '(1 4 7) '(2 5)) ; 3mod3 goes in partition 0, 4mod3 in partition 1, etc
;
;  (multi-partition #:partitions 3 #:source '(1 2 3 4 5 6 7) #:filter (curryr modulo 3)
;                   #:post-process-element (lambda (idx elem) (add1 elem))
;                   #:post-process-partition (lambda (lst) (map (curry * 2) lst)))
;     Returns: (values '(8 14) '(4 10 16) '(6 12)) ; each element +1 as list built, then later each item in list doubled
(define/contract (multi-partition #:partitions num-dests
                                  #:source source
                                  #:filter                 [chooser #f]
                                  #:post-process-partition [post-process-partition identity]
                                  #:post-process-element   [post-process-element   (lambda (idx elem) elem)]
                                  #:post-process-all-data  [post-process-all-data  vector->list]
                                  )
  (->* (#:partitions exact-positive-integer?
        #:source list?)
       (
        #:filter (-> any/c (or/c #f void? exact-nonnegative-integer?))
        #:post-process-partition (-> list? any/c)
        #:post-process-element   (-> exact-nonnegative-integer? any/c any/c)
        #:post-process-all-data  (-> vector? any)
        )
       any
       )
  (cond [(= 1 num-dests) source]
        [else
         ;;    The user can (and usually will) specify a function to
         ;;    split a list up.  If they don't specify one then by
         ;;    default we'll deal elements into each bin one after the
         ;;    next.
         (define index-chooser
           (or chooser
               (let-values ([(more? next) (sequence-generate (in-naturals))])
                 (lambda (x) (modulo (next) num-dests)))))

         (define max-idx (sub1 num-dests))
         (define results (make-vector num-dests '()))

         (for ((element source))
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
                         (reverse (vector-ref results i)))))
         (post-process-all-data results)]))

;;----------------------------------------------------------------------

(define/contract (pick lst)
  (-> (non-empty-listof any/c) any/c)
  (list-ref lst (random (length lst))))
