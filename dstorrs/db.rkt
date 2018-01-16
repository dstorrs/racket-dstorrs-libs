#lang at-exp racket

(require db
         dstorrs/utils
         dstorrs/list-utils
         dstorrs/sql
         dstorrs/try
         dstorrs/exceptions
         )


;======================================================================
;  A collection of convenience functions for working with the DB.
;
;  Re-exports everything from the dstorrs/sql library
;
;  Re-exports everything from the racket 'db' module except for
;  the 'disconnect' function.  That one is exported as 'db:disconnect'
;  so as not to clash with the disconnect method from the HTTP
;  libraries.
;======================================================================

(provide num-rows
         refine-db-exn
         query-rows-as-dicts
         query-row-as-dict
         query-maybe-row-as-dict
         query-flat
         ensure-disconnect
         maybe-disconnect
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

;;----------------------------------------------------------------------


;;----------------------------------------------------------------------


;  First, let's define some DB-specific exceptions
;
; Base of DB exn hierarchy
(struct exn:fail:db exn:fail () #:transparent)

; issues related to the DB itself
(struct exn:fail:db:create exn:fail:db () #:transparent) ; tried to create it, couldn't


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


;;----------------------------------------------------------------------

(define/contract (num-rows db table-name)
  (-> connection? (or/c symbol? string?) exact-integer?)
  (query-value db (~a "select count(1) from " table-name)))

;;----------------------------------------------------------------------


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

;; Function:  query-rows-as-dicts
;;
;; Summary:
;;
;;    Run a query against the database, return the result as a list of
;;    dicts. (By default hashes, but you can override to use some
;;    other form of dict if desired.)  You can also provide
;;    transformer functions to rewrite the data before the hashes are
;;    built or rewrite the hashes after they are built but before
;;    they are returned.
;;
;;    By default, if an exception is thrown from the query then an
;;    empty dict is returned.  If you want the exn to be re-raised
;;    then you can pass #:trap-exns #f.  In that case the exn will be
;;    sent to 'refine-db-exn' (see above) before being re-raised.
;;
;;
;; Contract:
;;
;;  (->* (list? connection? string?) ; keys, db handle, SQL string
;;       (
;;        #:wrapper          (-> connection (-> any) any) ; e.g. call-with-transaction/disconnect
;;        #:trap-exns?       boolean?
;;        #:dict-maker       procedure?
;;        #:transform-dict   procedure?
;;        #:transform-data   procedure?
;;        )
;;       #:rest list?
;;       (listof dict?))
;;
;;
;; The various procedures passed through the keywords are expected
;; to have the following contracts.  The contracts aren't actually
;; checked because raw lambdas and curried functions have no
;; available contract and so would fail the check.
;;
;;  #:wrapper        (-> (-> any) any)         ; takes a thunk, return value(s) not checked
;;  #:trap-exns?     boolean?                  ; return empty dict instead of throw? default: #t
;;  #:dict-maker     (-> (listof pair?) dict?) ; takes an assoc list, returns a dict
;;  #:transform-data (-> any/c any/c pair?)    ; transform the input of dict-maker
;;  #:transform-dict (-> dict? dict?)          ; transform the output of dict-maker
;;
;; The default dict-maker is make-hash, which produces a mutable
;; hash.  If you want an immutable hash then you can pass
;; make-immutable-hash to the #:dict-maker argument
;;
;;
;; Examples:
;;
;;    ;Run a query that needs no params. Result is a list of
;;    ;mutable hashes matching (hash/c symbol? any/c)  Note that
;;    ;the keys don't have to be the same as the field names
;; (query-rows-as-dicts '(chunk-hash chunk-num)
;;                      db-handle
;;                      "select hash, chunk_num from chunks")
;;
;;
;;    ;Run a query with params:
;; (query-rows-as-dicts '(chunk-hash chunk-num)
;;                      db-handle
;;                      "select hash, chunk_num from chunks where id = $1"
;;                      7)
;;
;;
;;    ;Same as the previous, except the arg is passed as a list.
;;    ;(i.e., it doesn't matter if you pass the args individually or
;;    ;in a list, so you can do whichever is more convenient)
;; (query-rows-as-dicts '(chunk-hash chunk-num)
;;                      db-handle
;;                      "select hash, chunk_num from chunks where id = $1"
;;                      '(7))
;;
;;
;;    ;Run a query but return the results as a list of 2-item lists
;;    ;instead of a list of hashes.
;; (query-rows-as-dicts '(chunk-hash chunk-num)
;;                      db-handle
;;                      "select hash, chunk_num from chunks where id = $1"
;;                      7
;;                      #:dict-maker flatten) ;; e.g. '(foo . 7) => '(foo 7)
;;
;;
;;    ;Run a query with params, add one to each chunk-num before
;;    ;returning the results.
;; (query-rows-as-dicts '(chunk-hash chunk-num)
;;                      db-handle
;;                      "select hash, chunk_num from chunks where id = $1"
;;                      7
;;                      #:transform-data (lambda (k v) (cons k (add1 v))))
;;
;;
;;    ;Run a query with params, convert the keys of the resulting
;;    ;hashes from symbols to strings before returning the results.
;;    ;(hash-keys->strings is defined in dstorrs/utils)
;;    ;
;;    ;NOTE: Obviously, this is contrived.  An easier solution would
;;    ;be to make the keys be strings in the first place.  That might
;;    ;not be straightforward, however, if the keys were being
;;    ;auto-generated somewhere else.
;; (query-rows-as-dicts '(chunk-hash chunk-num)
;;                      db-handle
;;                      "select hash, chunk_num from chunks where id = $1"
;;                      7
;;                      #:transform-dict hash-keys->strings)
;;
;;    ;Same as above, but run the query inside a transaction and then
;;    ;disconnect the db handle
;; (query-rows-as-dicts '(chunk-hash chunk-num)
;;                      db-handle
;;                      "select hash, chunk_num from chunks where id = $1"
;;                      7
;;                      #:transform-dict hash-keys->strings
;;                      #:wrapper call-with-transaction/disconnect)
;;
(define/contract (query-rows-as-dicts keys db sql
                                      #:wrapper        [wrapper    (lambda (db thnk) (thnk))]
                                      #:trap-exns?     [trap-exns? #t]
                                      #:dict-maker     [dict-maker make-hash]
                                      #:transform-dict [transform-dict identity]
                                      #:transform-data [transform-data cons]
                                      .
                                      params)
  (->* (list? connection? string?)
       (
        #:wrapper          (-> connection? (-> any) any)
        #:trap-exns?       boolean?
        #:dict-maker       procedure?
        #:transform-dict   procedure?
        #:transform-data   procedure?
        )
       #:rest list?
       (listof dict?))


  ; We flatten the parameters list as a convenience to the caller.
  ; That way you can do things like pass some arguments on their own
  ; and some as the result of map calls without having to ensure that
  ; it all ends up in one list.
  (define vals (flatten params))
  (define (v->d v)
    (vector->dict  ; defined in dstorrs/list-utils
     keys
     v
     #:dict-maker     dict-maker
     #:transform-data transform-data
     #:transform-dict transform-dict)
    )
  (try [
        (map  v->d
              (wrapper
               db
               (thunk
                (if (null? vals)
                    (query-rows db sql)
                    (apply (curry query-rows db sql) vals)))))
        ]
       [catch (match-anything
               (cond [trap-exns? (list (dict-maker))]
                     [else (compose1 raise refine-db-exn)]))]))

;;----------------------------------------------------------------------

; query-row-as-dict keys
;
; Same as query-rows-as-dicts, but expects that it will find exactly
; one row, so it returns a dict instead of a list of dicts.  If
; query-row throws an exception (probably because there was no such
; row) then query-row-as-dict returns an empty dict, unless you call
; it with #:trap-exns? #f, in which case it throws an appropriate
; exn:fail:db:*
(define/contract (query-row-as-dict keys
                                    db
                                    sql
                                    #:wrapper    [wrapper (lambda (db thnk) (thnk))]
                                    #:trap-exns? [trap-exns? #t]
                                    #:dict-maker [dict-maker make-hash]
                                    #:transform-dict [transform-dict identity]
                                    #:transform-data [transform-data cons]
                                    .
                                    params
                                    )
  (->* ((non-empty-listof any/c) connection? string?)
       (
        #:wrapper  (-> connection? (-> any))
        #:trap-exns? boolean?
        #:dict-maker (-> (listof pair?) dict?)   ; takes an assoc list, returns a dict
        #:transform-data (-> any/c any/c pair?)  ; transform the input of dict-maker
        #:transform-dict (-> dict? dict?)        ; transform the output of dict-maker
        )
       #:rest (listof any/c)
       dict?)

  (define (v->d v)
    (vector->dict  ; defined in dstorrs/list-utils
     keys
     v
     #:dict-maker     dict-maker
     #:transform-data transform-data
     #:transform-dict transform-dict)
    )
  (try [
        (define vals (flatten params))
        (v->d
         (wrapper db
                  (thunk
                   (if (null? vals)
                       (query-row db sql)
                       (apply (curry query-row db sql) vals)))))
        ]
       [catch (match-anything
               (lambda (e)
                 (cond [trap-exns? (dict-maker)]
                       [else ((compose1 raise refine-db-exn) e)])))]))

;;----------------------------------------------------------------------

; query-maybe-row-as-dict keys
;
; Same as query-rows-as-dicts, but expects that there will be zero or
; one rows.  one row so it returns a dict instead of a list of dicts.
; If there is no such row then it returns an empty dict.
(define/contract (query-maybe-row-as-dict keys
                                          db
                                          sql
                                          #:wrapper        [wrapper    (lambda (db thnk) (thnk))]
                                          #:trap-exns?     [trap-exns? #t]
                                          #:dict-maker     [dict-maker make-hash]
                                          #:transform-dict [transform-dict identity]
                                          #:transform-data [transform-data cons]
                                          .
                                          params
                                          )
  (->* ((non-empty-listof any/c) connection? string?)
       (
        #:wrapper          (-> connection? (-> any) any)
        #:trap-exns?       boolean?
        #:dict-maker (-> (listof pair?) dict?)   ; takes an assoc list, returns a dict
        #:transform-data (-> any/c any/c pair?)  ; transform the input of dict-maker
        #:transform-dict (-> dict? dict?)        ; transform the output of dict-maker
        )
       #:rest (listof any/c)
       dict?)

  (define (v->d v)
    (vector->dict  ; defined in dstorrs/list-utils
     keys
     v
     #:dict-maker     dict-maker
     #:transform-data transform-data
     #:transform-dict transform-dict)
    )
  (try [
        (define vals (flatten params))
        (define res (wrapper db
                             (thunk
                              (if (null? vals)
                                  (query-maybe-row db sql)
                                  (apply (curry query-maybe-row db sql) vals)))))
        (cond [(perl-false? res) (dict-maker)] ; #f or '()
              [else (v->d res)])
        ]
       [catch (match-anything
               (lambda (e)
                 (cond [trap-exns? (dict-maker)]
                       [else ((compose1 raise refine-db-exn) e)])))]))


;;----------------------------------------------------------------------

;  (query-flat db sql [vals '()] [converter vector->list])
;
; Performs a query-rows using the specified db, sql, and vals.  Maps
; the converter function over the results, then flattens whatever
; comes back and returns the resulting list.
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

; (define/contract (ensure-disconnect db thnk [wrapper unwrap-val])
;
; Calls the specified thunk.  Exceptions are trapped, sent to
; refine-db-exn, and re-raised.  Regardless of whether there is an
; exception, the db handle is guaranteed to be closed after this
; function completes.
;
; Args are:  database connection, thunk, optionally a function of two arguments
;
; NOTE:  See call-with-transaction/disconnect below.
;
(define/contract (ensure-disconnect db thnk [wrapper (lambda (db thnk) (unwrap-val thnk))])
  (->* (connection? (-> any)) ((-> connection? (-> any))) any)

  (try [(wrapper db thnk)]
       [catch (match-anything (lambda (e) (raise (refine-db-exn e))))]
       [finally
        ;(say "before disconnecting.  DB is connected?: " (connected? db))
        (disconnect db)
        ;(say "after disconnecting.  DB is connected?: " (connected? db))
        ]))

;;----------------------------------------------------------------------

;;  A variant on ensure-disconnect.  If you tell it to disconnect then
;;  it guarantees it will disconnect, even in the presence of
;;  exceptions.  If you tell it not to disconnect then it won't and
;;  exceptions will propagate as normal.  Useful when you have a
;;  function that creates a db handle and then disconnects it unless
;;  one is supplied in the args.
;;
;;  Aside from the #:disconnect keyword this accepts the same params
;;  as ensure-disconnect
(define/contract (maybe-disconnect db thnk #:disconnect disconnect? [wrapper (lambda (db thnk) (unwrap-val thnk))])
  (->* (connection? (-> any) #:disconnect boolean?) ((-> connection? (-> any))) any)

  ((if disconnect? ensure-disconnect (lambda (db thnk) (thnk)))
   db
   thnk))

;;----------------------------------------------------------------------

;(define/contract (call-with-transaction/disconnect db thnk)
;
; Shorthand for: (ensure-disconnect db thnk call-with-transaction))
(define/contract (call-with-transaction/disconnect db thnk)
  (-> connection? (-> any) any)

  (ensure-disconnect db thnk call-with-transaction))

;;----------------------------------------------------------------------
