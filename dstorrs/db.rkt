#lang at-exp rackjure

(require db
         dstorrs/utils
         dstorrs/list-utils
         dstorrs/sql
         dstorrs/exceptions
         )


(struct exn:fail:db:num-rows exn:fail (expected got) #:transparent)
(struct exn:fail:db:num-rows:zero exn:fail:db:num-rows () #:transparent)
(struct exn:fail:db:num-rows:many exn:fail:db:num-rows () #:transparent)

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
;;                      "select hash, chunk_num from chunks where id = $1")
;;                      7)
;;
;; ;Run a query but return the results as a list of 2-item lists
;; ;instead of a list of hashes.
;; (query-rows-as-dicts '(chunk-hash chunk-num)
;;                      db-handle
;;                      "select hash, chunk_num from chunks where id = $1")
;;                      7
;;                      #:dict-maker flatten ;; e.g. '(foo . 7) => '(foo 7)
;;
;; ;Run a query with params, add one to each chunk-num before
;; ;returning the results.
;; (query-rows-as-dicts '(chunk-hash chunk-num)
;;                      db-handle
;;                      "select hash, chunk_num from chunks where id = $1")
;;                      7
;;                      #:transform-data (lambda (k v) (cons k (add1 v)))
;;
;; ;Run a query with params, convert the keys from symbols to
;; ;strings before returning the results.
;; (query-rows-as-dicts '(chunk-hash chunk-num)
;;                      db-handle
;;                      "select hash, chunk_num from chunks where id = $1")
;;                      7
;;                      #:transform-dict (lambda (d)
;;                                         (for/hash ((k (hash-keys d)))
;;                                           (values (symbol->string k) (hash-ref d k)))
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
       list?)

  ;;  #:dict-maker (-> (listof pair?) dict?)   ; takes an assoc list, returns a dict
  ;;  #:transform-data (-> any/c any/c pair?)  ; transform the input of dict-maker
  ;;  #:transform-dict (-> dict? dict?)        ; transform the output of dict-maker
  ;;  )
  ;; #:rest (listof any/c)
  ;; (listof dict?))

  (define vals (flatten params))
  (define (v->d v)
    (vector->dict
     keys
     v
     #:dict-maker     dict-maker
     #:transform-data transform-data
     #:transform-dict transform-dict)
    )
  (say "get-rows-as-dicts, vals: " vals)
  (say "db: " db)
  (say "sql: " sql)
  (map  v->d
        (if (null? vals)
            (query-rows db sql)
            (apply (curry query-rows db sql) vals))))

;;----------------------------------------------------------------------

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
       (list?
        #:dict-maker (-> (listof pair?) dict?)   ; takes an assoc list, returns a dict
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


(provide (all-defined-out)
         (except-out (all-from-out db) disconnect)
         (all-from-out dstorrs/sql)
         )
