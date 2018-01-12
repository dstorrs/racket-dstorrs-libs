#lang at-exp racket

(require db
         dstorrs/utils
         dstorrs/list-utils
         dstorrs/sql
         dstorrs/try
         dstorrs/exceptions
         )


; Base of DB exn hierarchy
(struct exn:fail:db exn:fail () #:transparent)

; DB couldn't be created
(struct exn:fail:db:create exn:fail:db () #:transparent)

; issues related to a single row; doesn't exist (when it should), does
; exist (when it shouldn't)
(struct exn:fail:db:row exn:fail:db () #:transparent)
(struct exn:fail:db:row:not-exists exn:fail:db:row () #:transparent)
(struct exn:fail:db:row:exists exn:fail:db:row () #:transparent)

; issues related to multiple rows. NB: exn:fail:db:num-rows:zero and
; exn:fail:db:row:not-exists cover the same ground. Which one to use
; is a matter of preference and clarity in the specific situation
(struct exn:fail:db:num-rows exn:fail:db (expected got) #:transparent)
(struct exn:fail:db:num-rows:zero exn:fail:db:num-rows () #:transparent)
(struct exn:fail:db:num-rows:many exn:fail:db:num-rows () #:transparent)

; refine-db-exn
;
; This takes an arbitrary exception and, if it's related to a failed
; database call, attempts to turn it into a more specific exception
; from the list above.  (e.g., when you do a query-row (as opposed to
; query-maybe-row) and there is no such row in the DB then an exn:fail
; is returned containing a message about how there was no such row.
; This function will take that exn:fail and return an
; exn:fail:db:row:not-exists That way, whatever function caused the
; error can have error checking that says (when
; (exn:fail:db:row:not-exists?) ...) instead of having to do a
; complicated and hard-to-read regexp match.
;
; Note that the exception is **RETURNED**, not raised.  The calling
; code can then raise it, do further refinement on it, etc.  A normal
; way of calling this code would be:
;
;    (raise (refine-db-exn original-exn))
;
(define/contract (refine-db-exn e)
  (-> exn? exn?)

  (define msg (exn-message e))

  (define wrong-number-of-rows (pregexp
                                @~a{returned wrong number of rows.+?expected:\s+(\d+).+?got:\s+(\d+)}
                                ))

  (define num string->number) ;; for convenience

  (match msg
    [(regexp wrong-number-of-rows (list _ expected got))
     (create-exn (if (= (num got) 0)
                     exn:fail:db:num-rows:zero
                     exn:fail:db:num-rows:many
                     )
                 msg (num expected) (num got))]

    [_ e])
  )

;;----------------------------------------------------------------------

;;    Run a query against the database, return the result as a
;;    list of dicts. (By default hashes, but you can override
;;    if desired.)  You can also provide transformer functions
;;    to rewrite the data before the hashes are built or
;;    rewrite the results before they are returned.
;;
;;
;; ;Run a query that needs no params. Result is a list of
;; ;mutable hashes matching (hash/c symbol? any/c)
;; (query-rows-as-dicts '(chunk-hash chunk-num)
;;                      db-handle
;;                      "select hash, chunk_num from chunks")
;;
;; ;Run a query with params
;; (query-rows-as-dicts '(chunk-hash chunk-num)
;;                      db-handle
;;                      "select hash, chunk_num from chunks where id = $1"
;;                      7)
;;
;; ;Same as the previous, except the arg is passed as a list
;; (query-rows-as-dicts '(chunk-hash chunk-num)
;;                      db-handle
;;                      "select hash, chunk_num from chunks where id = $1"
;;                      '(7))
;;
;; ;Run a query but return the results as a list of 2-item lists
;; ;instead of a list of hashes.
;; (query-rows-as-dicts '(chunk-hash chunk-num)
;;                      db-handle
;;                      "select hash, chunk_num from chunks where id = $1"
;;                      7
;;                      #:dict-maker flatten) ;; e.g. '(foo . 7) => '(foo 7)
;;
;; ;Run a query with params, add one to each chunk-num before
;; ;returning the results.
;; (query-rows-as-dicts '(chunk-hash chunk-num)
;;                      db-handle
;;                      "select hash, chunk_num from chunks where id = $1"
;;                      7
;;                      #:transform-data (lambda (k v) (cons k (add1 v))))
;;
;; Run a query with params, convert the keys from symbols to strings
;; before returning the results.  (hash-keys->strings is defined in
;; dstorrs/utils)
;;
;; (query-rows-as-dicts '(chunk-hash chunk-num)
;;                      db-handle
;;                      "select hash, chunk_num from chunks where id = $1"
;;                      7
;;                      #:transform-dict hash-keys->strings)
(define/contract (query-rows-as-dicts keys db sql
                                      #:dict-maker     [dict-maker make-hash]
                                      #:transform-dict [transform-dict identity]
                                      #:transform-data [transform-data cons]
                                      .
                                      params)
  (->* (list? connection? string?)
       (
        #:dict-maker procedure?
        #:transform-dict procedure?
        #:transform-data procedure?
        )
       #:rest list?
       (listof dict?))

  ;; The various procedures passed through the keywords are expected
  ;; to have these contracts.  The contracts aren't actually checked
  ;; because raw lambdas and curried functions have no available
  ;; contract and so would fail the check.
  ;;
  ;;  #:dict-maker     (-> (listof pair?) dict?)   ; takes an assoc list, returns a dict
  ;;  #:transform-data (-> any/c any/c pair?)      ; transform the input of dict-maker
  ;;  #:transform-dict (-> dict? dict?)            ; transform the output of dict-maker
  ;;
  ;; The default dict-maker is make-hash, which produces a mutable hash.

  ; We flatten the parameters list as a convenience to the caller.
  ; That way you can do things like pass some arguments on their own
  ; and some as the result of map calls without having to ensure that
  ; it all ends up in one list.
  (define vals (flatten params))
  (define (v->d v)
    (vector->dict
     keys
     v
     #:dict-maker     dict-maker
     #:transform-data transform-data
     #:transform-dict transform-dict)
    )
  (map  v->d
        (if (null? vals)
            (query-rows db sql)
            (apply (curry query-rows db sql) vals))))

;;----------------------------------------------------------------------

; query-row-as-dict keys
;
; Same as query-rows-as-dicts, but expects that it will find exactly
; one row, so it returns a dict instead of a list of dicts.  If
; query-row throws an exception (probably because there was no such
; row) then query-row-as-dict returns an empty dict, unless you call
; it with #:throw-on-exn? #t, in which case it throws an appropriate
; exn:fail:db:*
(define/contract (query-row-as-dict keys
                                    db
                                    sql
                                    #:dict-maker [dict-maker make-hash]
                                    #:transform-dict [transform-dict identity]
                                    #:transform-data [transform-data cons]
                                    .
                                    vals
                                    )
  (->* ((non-empty-listof any/c) connection? string?)
       (#:dict-maker (-> (listof pair?) dict?)   ; takes an assoc list, returns a dict
        #:transform-data (-> any/c any/c pair?)  ; transform the input of dict-maker
        #:transform-dict (-> dict? dict?)        ; transform the output of dict-maker
        )
       #:rest (listof any/c)
       dict?)

  (define res (query-rows-as-dicts keys db sql vals
                                   #:dict-maker     dict-maker
                                   #:transform-dict transform-dict
                                   #:transform-data transform-data
                                   ))
  (if (null? res)
      (dict-maker)
      (car res)))

;;----------------------------------------------------------------------

; query-maybe-row-as-dict keys
;
; Same as query-rows-as-dicts, but expects that there will be zero or one rows.
; one row so it returns a dict instead of a list of dicts
(define/contract (query-maybe-row-as-dict keys
                                          db
                                          sql
                                          #:dict-maker     [dict-maker make-hash]
                                          #:transform-dict [transform-dict identity]
                                          #:transform-data [transform-data cons]
                                          .
                                          args
                                          )
  (->* ((non-empty-listof any/c) connection? string?)
       (#:dict-maker (-> (listof pair?) dict?)   ; takes an assoc list, returns a dict
        #:transform-data (-> any/c any/c pair?)  ; transform the input of dict-maker
        #:transform-dict (-> dict? dict?)        ; transform the output of dict-maker
        )
       #:rest (listof any/c)
       dict?)

  ;;    If they passed the arguments as a list it will be received
  ;;    with an extra list wrapper, e.g. '(("foo")). Unwrap it to
  ;;    '("foo")
  (define vals (flatten args))
  (define res
    (if (null? vals)
        (query-maybe-row db sql)
        (apply (curry query-maybe-row db sql) vals)))

  (if (not res)
      (dict-maker)
      (vector->dict keys
                    res
                    #:dict-maker dict-maker
                    #:transform-data transform-data
                    #:transform-dict transform-dict)))

;;----------------------------------------------------------------------

(define/contract (query-flat db sql [vals '()] [converter vector->list])
  (->* (connection? string?) (any/c procedure?) list?)
  (flatten/convert converter
                   (if (null? vals)
                       (query-rows db sql)
                       (apply (curry query-rows db sql)
                              (if (null? vals)
                                  vals
                                  (autobox vals))))))

;;----------------------------------------------------------------------

(define/contract (call-with-transaction/disconnect db thnk)
  (-> connection? (-> any) any)

  (try [(call-with-transaction db thnk)]
       [catch (match-anything (lambda (e) (raise (refine-db-exn e))))]
       [finally
        ;(say "before disconnecting.  DB is connected?: " (connected? db))
        (disconnect db)
        ;(say "after disconnecting.  DB is connected?: " (connected? db))
        ]))

;;----------------------------------------------------------------------


(provide refine-db-exn
         query-rows-as-dicts
         query-row-as-dict
         query-maybe-row-as-dict
         query-flat
         call-with-transaction/disconnect
         (except-out (all-from-out db) disconnect)
         (prefix-out db: disconnect)
         (all-from-out dstorrs/sql)
         (struct-out exn:fail:db)
         (struct-out exn:fail:db:create)
         (struct-out exn:fail:db:row:not-exists)
         (struct-out exn:fail:db:row:exists)
         (struct-out exn:fail:db:num-rows)
         (struct-out exn:fail:db:num-rows:zero)
         (struct-out exn:fail:db:num-rows:many)
         )
