#lang racket/base

(require "deprecated/list-utils.rkt"
         (only-in "hash.rkt"
                  hash->keyword-apply)
         racket/bool
         racket/contract/base
         racket/contract/region
         racket/dict
         racket/function
         racket/list
         racket/match
         racket/sequence
         racket/stream)

(provide (all-defined-out)
         hash->keyword-apply
         (all-from-out "deprecated/list-utils.rkt")
         (struct-out dict-disjunction))

;;    Parameters:
;; *) current-dict-maker-function : customize what vector->dict and list->dict do
;; *) current-transform-data-function
;; *) current-transform-data-function
;; *) make-transform-data-func
;; *) make-transform-data-func*

;;    Functions:
;; *) alist->hash alist : turn an alist like '((a . 1) (b . 2)) into an immutable hash
;; *) atom? : true if something is not a pair. (symbol, number, vector...)
;; *) autobox : ensure that its argument is a list. If not, returns (list arg)
;; *) compose-fifo : like 'compose' but applies args from left to right, not right to left
;; *) disjunction : find the elements of one dict that are not in the other
;; *) first*  : return the leftmost atom in a list
;; *) in-range-inc : inclusive ranges
;; *) insert-at    : (insert-at '(a b c e) '(d) 3) returns '(a b c d e)
;; *) find-contiguous-runs : search a list for contiguous segments,
;;     return a list of sublists
;; *) L : alias for 'list'  (NOTE: deprecated!)
;; *) list->values  ; because it's dumb that this doesn't exist
;; *) list-remf* filter all desired elements out of a list, by default #<void>
;; *) list/not-null? : is it a pair and not '()? NB: checks for pair,
;;     not list, so it treats '(x . y) as a list
;; *) member-rec : finds matching elements in sublist as well as main list
;; *) multi-partition : like the standard partition, but accepts multiple destination lists
;; *) remove-nulls : filter '()s out of a list
;; *) safe-first, safe-rest : first and rest, but they return specified value when given '()
;; *) safe-first* : like safe-first, but return the leftmost atom
;; *) slice :  return elements of a list from index x to index y, or as much as remains
;; *) sort-num, sort-str, sort-sym, sort-bool : shorthand for sort with appropriate comparator
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
  (->* () () #:rest (and/c list? (compose even? length)) procedure?)

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

;;----------------------------------------------------------------------

;  This is similar to make-transform-data-func except that it matches
;  all clauses in order.  Comparison using data returned from a
;  Postgres DB.
;
;     ;   Nothing that comes back from a PG query is a list, so the
;     ;   second clause will never match.
;     (make-transform-data-func pg-array? pg-array->list
;                               (curry equal? (list sql-null)) '())
;
;     ;   Here, the pg-arrays will be converted to lists and then the
;     ;   lists will be checked to see if they consist of only a sql-null.
;     ;   This would be a common case when using LEFT JOIN.
;     (make-transform-data-func* pg-array? pg-array->list
;                                (curry equal? (list sql-null)) '())
(define/contract (make-transform-data-func* . args)
  (->* ()
       ()
       #:rest (and/c list? (compose even? length))
       (-> any/c any/c pair?))

  ;    Example:
  ;
  ;    (define cleaner (make-transform-data-func* pg-array? pg-array->list
  ;                                               #t    'true
  ;                                               void? #f
  ;                                               (curry equal? (list sql-null)) '())))
  ;
  ;    (cleaner 'friends (pg-array "bob" "charlie")   => (cons 'friends '("bob" "charlie"))
  ;    (cleaner 'true?   #t                           => (cons 'true? 'true)
  ;    (cleaner 'nothing (void)                       => (cons 'nothing #f)
  ;    (cleaner 'false?  #f                           => (cons 'false? #f)  ; unchanged
  ;    (cleaner 'phones  (pg-array sql-null))         => (cons 'phones '()) ; two changes!
  ;
  (define pred/func-pairs
    (step-by-n
     (lambda (p f)
       (define pred (if (procedure? p) p (curry equal? p))) ; e.g. pg-array?
       (define func (if (procedure? f) f (lambda _ f)))     ; e.g. pg-array->list
       (cons pred func))
     args))

  (lambda (key orig-val)  ; e.g.  'phones  (pg-array sql-null)
    (for/fold ([result (cons key orig-val)])
              ([pred/func pred/func-pairs]) ; test against all predicate/func pairs in turn
      (define val (cdr result))
      (match pred/func
        [(cons pred func)
         (cond [(pred val) (cons key (func val))]
               [else       result])]))))

;;----------------------------------------------------------------------

(define/contract (safe-first l [default '()])
  (->* (list?) (any/c) any/c)
  (cond [(null? l) default]
        [else (first l)]))

(define/contract (safe-rest  l [default '()])
  (->* (list?) (any/c) any/c)
  (cond [(null? l) default]
        [else (rest l)]))

;;----------------------------------------------------------------------

; first*
;
; When given a non-null list it returns the leftmost atom in the list.
; Example: (first* '((8))) returns 8.
;
; When given a non-list or null list, it throws the normal exception
; that 'first' would throw on such data.
(define/contract (first* lst)
  (-> list? any/c)

  (cond [(or (not (list? lst)) (null? lst))  (first lst)] ; deliberately fail
        [(list? (car lst)) (first* (car lst))]
        [else (first lst)]))

;;----------------------------------------------------------------------

; (safe-first* lst default)
;   (->* (list?) (any/c) any/c)
; default = '()
;
; When given a non-null list it returns the leftmost atom in the list.
; Example: (safe-first* '((8))) returns 8.
;
; When given a non-list, it throws the normal exception that 'first'
; would throw on such data.
;
; When given a null list it returns the specified default value or '()
; if none was supplied.
(define/contract (safe-first* lst [default '()])
  (->* (list?) (any/c) any/c)

  (cond [(not (list? lst)) (first lst)] ; deliberately fail so as to throw standard exception
        [(null? lst) default]
        [(list? (car lst)) (safe-first* (car lst))]
        [else (first lst)]))

;;----------------------------------------------------------------------

(define atom? (negate pair?))

;;----------------------------------------------------------------------

(define/contract (autobox x)
  (-> any/c list?)
  (if (list? x) x (list x)))

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
(define/contract (list-remf* #:pred [pred void?] . l)
  (->* () (#:pred predicate/c) #:rest list? list?)
  (remf* pred l))

;;----------------------------------------------------------------------

(define/contract (list/not-null? l)
  (-> any/c boolean?)
  (and (not (atom? l)) (not (null? l))))

;;----------------------------------------------------------------------

; recursively remove items from a list
(define (remove-duplicates/rec  lst [same? equal?] #:key [extract-key identity])
  (define (_helper lst seen)
    (match lst
      ['()
       (values '() seen)]
      [(list  head tail ...)
       #:when (list? head)
       (define-values (sub-headres sub-headseen) (_helper head seen))
       (define-values (sub-tailres sub-tailseen) (_helper tail sub-headseen))

       (values (cons sub-headres sub-tailres) sub-tailseen)]
      [(list  head tail ...)
       #:when (not (member (extract-key head) seen same?))
       (define-values (tail-result tail-seen)
         (_helper tail (cons (extract-key head) seen)))
       (values (cons head tail-result)
               tail-seen)]
      [(list  head tail ...)
       (_helper tail seen)]))

  (define-values (result seen) (_helper lst '()))
  result)

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

(define/contract (sort-num lst #:key [key identity] #:cache-keys? [cache? #f] #:asc? [asc? #t])
  (->* (list?) (#:key (-> any/c any/c) #:cache-keys? boolean? #:asc? boolean?) list?)
  (define comparator (if asc? < >))
  (sort lst comparator #:key key #:cache-keys? cache?))

(define/contract (sort-str lst #:key [key identity] #:cache-keys? [cache? #f] #:asc? [asc? #t])
  (->* (list?) (#:key (-> any/c any/c) #:cache-keys? boolean? #:asc? boolean?) list?)
  (define comparator (if asc? string<?  (negate string<?)))
  (sort lst comparator #:key key #:cache-keys? cache?))

(define/contract (sort-sym lst #:key [key identity] #:cache-keys? [cache? #f] #:asc? [asc? #t])
  (->* (list?) (#:key (-> any/c any/c) #:cache-keys? boolean? #:asc? boolean?) list?)
  (define comparator (if asc? symbol<?  (negate symbol<?)))
  (sort lst comparator #:key key #:cache-keys? cache?))

(define/contract (sort-bool lst #:key [key identity] #:cache-keys? [cache? #f] #:asc? [asc? #t])
  (->* (list?) (#:key (-> any/c any/c) #:cache-keys? boolean? #:asc? boolean?) list?)
  (sort lst #:key key #:cache-keys? cache?
        (if asc?
            (lambda (x y) (not (false? x))) ; sort #t first
            (lambda (x y) (false? x)))))

(define/contract (sort-smart lst #:key [key identity] #:cache-keys? [cache? #f] #:asc? [asc? #t])
  (->* (list?) (#:key (-> any/c any/c) #:cache-keys? boolean? #:asc? boolean?) list?)
  (cond [(null? lst) '()]
        [else
         (define func
           (match (key (first lst))
             [(? symbol?) sort-sym]
             [(? string?) sort-str]
             [(? number?) sort-num]
             [(? boolean?) sort-bool]
             [_ (raise-arguments-error 'sort-smart
                                       "all elements of list must be symbol, string, number, or bool"
                                       "args list" lst)]))
         (func lst #:key key #:cache-keys? cache? #:asc? asc?)]))

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
(define/contract (get s keys [def the-unsupplied-arg])
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
                 [(unsupplied-arg? def) (raise e)]
                 [(not (exn:fail:contract? e)) (raise e)]
                 [(regexp-match #px"(no value found for key|index (too large|is out of range))"
                                (exn-message e))
                  def
                  ]
                 [else (raise e)]))])
           (foldl get-once s (autobox keys)))]))

;;----------------------------------------------------------------------

(define/contract (in-range-inc x [y #f] [step 1])
  (->* (real?) (real? real?) stream?)
  (if y
      (in-range x (+ y step) step)
      (in-range   (add1 x))))

;;----------------------------------------------------------------------

(define (list->assoc-list lst)
  (step-by-n cons lst))

;;----------------------------------------------------------------------

; This is a trampoline to list->dict.  It simply calls vector->list and passes that and
; its arguments on to list->dict.
(define/contract (vector->dict raw-keys
                               data
                               #:make-keys      [key-maker      #f]
                               #:transform-data [transform-data (current-transform-data-function)]
                               #:dict-maker     [dict-maker     (current-dict-maker-function)]
                               #:transform-dict [transform-dict (current-transform-dict-function)])
  (->* (list? vector)                              ;; keys and data
       (
        #:make-keys      (or/c #f (-> any/c any/c)); generate the keys list based on data
        #:transform-data (-> any/c any/c pair?)    ; transform the input of dict-maker
        #:dict-maker     (-> (listof pair?) any/c) ; takes an assoc list, returns something
        #:transform-dict (-> any/c any)            ; transform the output of dict-maker
        )
       any)

  (if (not data)
      (dict-maker);; Makes handling DB queries easier
      (list->dict raw-keys
                  (vector->list    data)
                  #:dict-maker     dict-maker
                  #:transform-dict transform-dict
                  #:transform-data transform-data
                  #:make-keys      key-maker)))

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
                             #:dict-maker     [dict-maker     (current-dict-maker-function)]
                             #:transform-dict [transform-dict (current-transform-dict-function)]
                             #:transform-data [transform-data (current-transform-data-function)]
                             #:make-keys      [key-maker      #f]
                             )
  (->* (list? list?)                               ;; keys and data
       (
        #:make-keys      (or/c #f (-> any/c any/c)); generate the keys list based on data
        #:transform-data (-> any/c any/c pair?)    ; transform the input of dict-maker
        #:dict-maker     (-> (listof pair?) any/c) ; takes an assoc list, returns something
        #:transform-dict (-> any/c any)            ; transform the output of dict-maker
        )
       any)

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
;; Keyword arguments are:
;;
;;    #:key Function to use in order to generate the values that
;;    define what a run is.  By default this is 'identity'.  You can
;;    pass a function of (-> any/c any/c) to use instead.
;;
;;    #:op The function to use for determining if two elements are
;;    contiguous.  By default this is:
;;
;;      (lambda (a b) (= (add1 (extract-value a) (extract-value b))))
;;
;;    where 'extract-value is the function that was passed to #:key
;;    (or its default value).  If that doesn't work for your use case,
;;    you can pass a function of:
;;
;;      (-> any/c any/c boolean?) instead.
;;
;; Example:
;;
;;     (find-contiguous-runs (list (hash 'age 17) (hash 'age 18) (hash 'age 27))
;;                           #:key (curryr hash-ref 'age))
;;        =>  (list (list (hash 'age 17) (hash 'age 18))
;;                  (list (hash 'age 27)))
;;
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
         (define-values (ignore final result)
           (for/fold ((prev (car data))
                      (current-run  (list (car data)))
                      (result '())
                      )
                     ((curr (cdr data)))

             ;; if curr is contiguous with prev, add curr to current-run
             ;; if curr not contiguous, add acc to result and clear current-run for next time

             (define is-contiguous (contiguous? prev curr))
             (values curr                  ;; the one we just processed becomes 'prev(ious)'
                     (if is-contiguous     ;; add to or clear accumulator of current run
                         (cons curr current-run)
                         (list curr))      ;; start a new run
                     ;;
                     (if is-contiguous     ;; add accumulator to result if the run is over
                         result
                         (cons (reverse current-run) result)
                         ))
             ))
         (reverse (cons (reverse final) result))]))

;;----------------------------------------------------------------------

(define/contract (flatten/convert func lst)
  (-> procedure? list? list?)
  ((compose flatten (curry map func)) lst))

;;----------------------------------------------------------------------

(define/contract (unique lst [same? equal?] #:key [key-maker identity])
  (->* (list?) ((-> any/c any/c boolean?) #:key (-> any/c any/c)) list?)
  (remove-nulls
   (remove-duplicates lst same? #:key key-maker)))

;;----------------------------------------------------------------------

(define/contract (unwrap-list lst)
  (-> list? list?)
  (match lst
    [(list (list item ...)) item]
    [else lst]))

;;----------------------------------------------------------------------

(define/contract (symbols->keywords lst)
  (-> (listof symbol?) (listof keyword?))
  (map (compose string->keyword symbol->string) (sort lst symbol<?)))

;;----------------------------------------------------------------------

;   multi-partition
;
; This is like group-by, but with some additional properties and
; capabilities.  Specifically:
;
;    * Items will come back in the order you want them to, not in the
;    order they happen to occur in the source.  This is useful if you
;    have different kinds of elements in a list -- for example, given
;    a function that might return a valid result or throw one of
;    various kinds of exceptions, you can multi-partition the results
;    such that all the valid ones are in partition #1 and each of the
;    various kinds of exceptions are all in expected slots.
;
;    * You can separately post process:
;
;        * each element
;
;        * each partition
;
;        * all data
;
;    Example:
;
;  (multi-partition #:partitions 3 #:source '(1 2 3 4 5 6 7) #:filter (curryr modulo 3))
;
;     Returns: (values '(3 6) '(1 4 7) '(2 5)) ; 3mod3 goes in
;     partition 0, 4mod3 in partition 1, etc
;
;
;  (multi-partition #:partitions 3 #:source '(1 2 3 4 5 6 7) #:filter (curryr modulo 3)
;                   #:post-process-element (lambda (idx elem) (add1 elem))
;                   #:post-process-partition (lambda (lst) (map (curry * 2) lst)))
;
;     Returns: (values '(8 14) '(4 10 16) '(6 12)) ; each element +1
;     as list built, then later each item in list doubled
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

;;   Choose a random element from a list
(define/contract (pick lst)
  (-> (non-empty-listof any/c) any/c)
  (list-ref lst (random (length lst))))

;;----------------------------------------------------------------------

;;  Insert a list into another list at a specified position
(define/contract (insert-at orig-lst new-lst n)
  (-> list? list? natural-number/c list?)

  (when (> n (length orig-lst))
    (raise-arguments-error 'insert-at
                           "n must be <= the length of list"
                           "n" n
                           "original list" orig-lst
                           "new list" new-lst))

  (define-values (before after)
    (split-at orig-lst n))
  (append before new-lst after))
