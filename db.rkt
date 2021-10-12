#lang at-exp racket/base

(require (for-syntax racket/base)
         db
         racket/contract/base
         racket/contract/region
         racket/dict
         racket/format
         racket/function
         racket/list
         racket/match
         "exceptions.rkt"
         "list-utils.rkt"
         "sql.rkt"
         "try.rkt"
         (except-in "utils.rkt" hash->keyword-apply)) ; already required from list-utils.rkt

;======================================================================
;  A collection of convenience functions for working with the DB.
;
;  Re-exports everything from the handy/sql library
;
;  Re-exports everything from the racket 'db' module except for
;  the 'disconnect' function.  That one is exported as 'db:disconnect'
;  so as not to clash with the disconnect method from the HTTP
;  libraries.
;======================================================================

(provide num-rows
         refine-db-exn
         refine/raise
         query-rows-as-dicts
         query-row-as-dict
         query-maybe-row-as-dict
         query-flat
         ensure-disconnect
         maybe-disconnect
         disconnect-if
         call-with-transaction/disconnect
         (except-out (all-from-out db) disconnect) ; includes sql-null->false
         (prefix-out db: disconnect)
         (all-from-out "sql.rkt")
         (struct-out exn:fail:db)
         (struct-out exn:fail:db:create)
         (struct-out exn:fail:db:row:not-exists)
         (struct-out exn:fail:db:row:exists)
         (struct-out exn:fail:db:row:not-updated)
         (struct-out exn:fail:db:num-rows)
         (struct-out exn:fail:db:num-rows:zero)
         (struct-out exn:fail:db:num-rows:many)
         (struct-out exn:fail:db:constraint)
         (struct-out exn:fail:db:constraint:unique)
         (struct-out exn:fail:db:constraint:check)

         ;;   Parameters that allows you to customize how the various
         ;;   'query...-as-dict' functions work
         current-query-as-dict-dict-maker-function
         current-query-as-dict-transform-data-function
         current-query-as-dict-transform-dict-function

         make-transform-data-func ; imported from list-utils
         )

;;----------------------------------------------------------------------


(define current-query-as-dict-dict-maker-function
  (make-parameter (current-dict-maker-function))) ; cf in list-utils

(define current-query-as-dict-transform-data-function
  (make-parameter (current-transform-data-function))) ; cf list-utils

(define current-query-as-dict-transform-dict-function
  (make-parameter (current-transform-dict-function))) ; cf list-utils

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
(struct exn:fail:db:row:not-updated exn:fail:db:row () #:transparent)

; issues related to multiple rows. NB: exn:fail:db:num-rows:zero and
; exn:fail:db:row:not-exists cover the same ground. Which one to use
; is a matter of preference and clarity in the specific situation
(struct exn:fail:db:num-rows exn:fail:db (expected got) #:transparent)
(struct exn:fail:db:num-rows:zero exn:fail:db:num-rows () #:transparent)
(struct exn:fail:db:num-rows:many exn:fail:db:num-rows () #:transparent)

(struct exn:fail:db:constraint exn:fail:db (name) #:transparent)
(struct exn:fail:db:constraint:unique exn:fail:db:constraint () #:transparent)
(struct exn:fail:db:constraint:check  exn:fail:db:constraint () #:transparent)

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

  (define num string->number) ;; for convenience


  (define wrong-number-of-rows
    (pregexp
     @~a{returned wrong number of rows.+?expected:\s+(\d+).+?got:\s+(\d+)}
     ))

  (define unique-constraint
    (pregexp @~a{duplicate key value violates \w+ constraint "(\w+)".+}))

  (define check-constraint
    (pregexp
     @~a{^.+? new row for relation "\w+" violates check constraint "(\w+)".+}))

  (match msg
    [(regexp wrong-number-of-rows (list _ expected got))
     (create-exn (if (= (num got) 0)
                     exn:fail:db:num-rows:zero
                     exn:fail:db:num-rows:many
                     )
                 msg (num expected) (num got))]
    ;
    [(regexp unique-constraint (list _ constraint-name))
     (create-exn exn:fail:db:constraint:unique msg constraint-name)]
    ;
    [(regexp check-constraint (list _ constraint-name))
     (create-exn exn:fail:db:constraint:check msg constraint-name)]
    ;
    [_ e])
  )

;;----------------------------------------------------------------------

(define refine/raise (compose1 raise refine-db-exn))

;;----------------------------------------------------------------------

;; Function:  query-rows-as-dicts
;;
;; Summary:
;;
;;    Run a query against the database, return the result as a list of dicts. (By default
;;    hashes, but you can override to use some other form of dict if desired.)  You can
;;    also provide transformer functions to rewrite the data before the hashes are built
;;    or rewrite the hashes after they are built but before they are returned.
;;
;;    By default, if an exception is thrown from the query then an empty dict is returned.
;;    If you want the exn to be re-raised then you can pass #:trap-exns #f.  In that case
;;    the exn will be sent to 'refine-db-exn' (see above) before being re-raised.
;;
;;
;; Contract:
;;
;; (->* (list? connection? string?)
;;      (
;;       #:wrapper          (-> connection? (-> any) any)
;;       #:trap-exns?       boolean?
;;       #:dict-maker       (-> (listof pair?) dict?)
;;       #:transform-dict   (-> any/c any/c)
;;       #:transform-data   (-> any/c any/c pair?)
;;       #:post-process     (-> list? any)
;;       )
;;      #:rest list?
;;      any)
;;
;;
;; The default dict-maker is make-hash, which produces a mutable hash.  If you want an
;; immutable hash then you can pass make-immutable-hash to the #:dict-maker argument
;;
;;  NOTE: The function is called 'query-rows-as-DICTS' but it will actually allow you to
;;  return anything you want.  A common reason for this was that you want the hashes to be
;;  converted into structs before being returned.
;;
;;
;; Examples:
;;
;;    ;Run a query that needs no params. Result is a list of mutable hashes matching
;;    ;(hash/c symbol? any/c) Note that the keys are different from the field names.
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
;;    ;Run a query but return the results as a list of 2-item lists instead of a list of
;;    ;hashes.
;; (query-rows-as-dicts '(chunk-hash chunk-num)
;;                      db-handle
;;                      "select hash, chunk_num from chunks where id = $1"
;;                      7
;;                      #:dict-maker flatten) ;; e.g. '(foo . 7) => '(foo 7)
;;
;;
;;    ;Run a query with params, add one to each chunk-num before returning the results.
;;    ;In practice you should prefer to do this in the DB but this makes a clear example.
;; (query-rows-as-dicts '(chunk-hash chunk-num)
;;                      db-handle
;;                      "select hash, chunk_num from chunks where id = $1"
;;                      7
;;                      #:transform-data (lambda (k v) (cons k (add1 v))))
;;
;;
;;    ;Run a query with params, convert the results to structs.  hash->struct/kw is defined
;;    ;in handy/struct
;;
;; (struct person (name age))
;; (define (person/kw #:name name #:age age) (person name age))
;; (query-rows-as-dicts '(name age)
;;                      db-handle
;;                      "select name, age from users where id = $1"
;;                      #:transform-dict (curry hash->struct/kw person/kw)
;;                      7)
;;
;;    ;Same as above, but run the query inside a transaction and then
;;    ;disconnect the db handle
;; (query-rows-as-dicts '(name age)
;;                      db-handle
;;                      "select name, age from users where id = $1"
;;                      7
;;                      #:transform-dict (curry hash->struct/kw person/kw)
;;                      #:wrapper call-with-transaction/disconnect)
(define/contract (query-rows-as-dicts keys db sql
                                      #:wrapper        [wrapper    (lambda (db thnk) (thnk))]
                                      #:trap-exns?     [trap-exns? #f]
                                      #:dict-maker     [dict-maker (current-query-as-dict-dict-maker-function)] ; defined in list-utils
                                      #:transform-dict [transform-dict (current-query-as-dict-transform-dict-function)]
                                      #:transform-data [transform-data (current-query-as-dict-transform-data-function)]
                                      #:post-process   [post-processor identity]
                                      .
                                      params)
  (->* (list? connection? string?)
       (
        #:wrapper          (-> connection? (-> any) any)
        #:trap-exns?       boolean?
        #:dict-maker       (-> (listof pair?) dict?)
        #:transform-dict   (-> any/c any/c)
        #:transform-data   (-> any/c any/c pair?)
        #:post-process     (-> list? any)
        )
       #:rest list?
       any)


  ; We flatten the parameters list as a convenience to the caller.
  ; That way you can do things like pass some arguments on their own
  ; and some as the result of map calls without having to ensure that
  ; it all ends up in one list.
  (define vals (flatten params))
  (define (v->d v)
    (vector->dict  ; defined in handy/list-utils
     keys
     v
     #:dict-maker     dict-maker
     #:transform-data transform-data
     #:transform-dict transform-dict)
    )
  (try [
        (post-processor
         (map  v->d
               (wrapper
                db
                (thunk
                 (if (null? vals)
                     (query-rows db sql)
                     (apply (curry query-rows db sql) vals))))))
        ]
       [catch (match-anything
               (lambda (e)
                 (cond [trap-exns? '()]
                       [else (raise (refine-db-exn e))])))]))

;;----------------------------------------------------------------------

; query-row-as-dict keys
;
; Same as query-rows-as-dicts, but expects that it will find exactly
; one row, so it returns a dict instead of a list of dicts.  If
; query-row throws an exception (probably because there was no such
; row) then query-row-as-dict returns an empty dict, unless you call
; it with #:trap-exns? #f, in which case it throws an appropriate
; exn:fail:b:*
(define/contract (query-row-as-dict keys
                                    db
                                    sql
                                    #:wrapper    [wrapper (lambda (db thnk) (thnk))]
                                    #:trap-exns? [trap-exns? #t]
                                    #:dict-maker     [dict-maker (current-query-as-dict-dict-maker-function)] ; defined in list-utils
                                    #:transform-dict [transform-dict (current-query-as-dict-transform-dict-function)]
                                    #:transform-data [transform-data (current-query-as-dict-transform-data-function)]
                                    #:post-process   [post-processor identity]
                                    .
                                    params
                                    )
  (->* ((non-empty-listof any/c) connection? string?)
       (
        #:wrapper  (-> connection? (-> any))
        #:trap-exns? boolean?
        #:dict-maker (-> (listof pair?) dict?)     ; takes an assoc list, returns a dict
        #:transform-data (-> any/c any/c pair?)    ; transform the input of dict-maker
        #:transform-dict (-> dict? dict?)          ; transform the output of dict-maker
        #:post-process     (-> dict? any)          ; transform the final result to anything
        )
       #:rest (listof any/c)
       any)

  (define (v->d v)
    (vector->dict  ; defined in handy/list-utils
     keys
     v
     #:dict-maker     dict-maker
     #:transform-data transform-data
     #:transform-dict transform-dict)
    )
  (try [
        (define vals (flatten params))
        (post-processor
         (v->d
          (wrapper db
                   (thunk
                    (if (null? vals)
                        (query-row db sql)
                        (apply (curry query-row db sql) vals))))))
        ]
       [catch (match-anything
               (lambda (e)
                 (cond [trap-exns? (dict-maker)]
                       [else (refine/raise e)])))]))

;;----------------------------------------------------------------------

; query-maybe-row-as-dict keys
;
; Same as query-rows-as-dicts, but expects that there will be zero or
; one rows.  one row so it returns a dict instead of a list of dicts.
; If there is no such row then it returns an empty dict.
(define/contract (query-maybe-row-as-dict keys db sql
                                          #:wrapper        [wrapper    (lambda (db thnk) (thnk))]
                                          #:trap-exns?     [trap-exns? #f]
                                          #:dict-maker     [dict-maker (current-query-as-dict-dict-maker-function)] ; defined in list-utils
                                          #:transform-dict [transform-dict (current-query-as-dict-transform-dict-function)]
                                          #:transform-data [transform-data (current-query-as-dict-transform-data-function)]
                                          #:post-process   [post-processor identity]
                                          .
                                          params)
  (->* (list? connection? string?)
       (
        #:wrapper          (-> connection? (-> any) any)
        #:trap-exns?       boolean?
        #:dict-maker       (-> (listof pair?) dict?)
        #:transform-dict   (-> dict? dict?)
        #:transform-data   (-> any/c any/c pair?)
        #:post-process     (-> (listof dict?) any)
        )
       #:rest list?
       any)
  (define results (query-rows-as-dicts keys db sql
                                       #:wrapper        wrapper
                                       #:trap-exns?     trap-exns?
                                       #:dict-maker     dict-maker
                                       #:transform-dict transform-dict
                                       #:transform-data transform-data
                                       #:post-process   post-processor
                                       params))
  (if (null? results)
      #f
      (first results)))

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
; Args are: database connection, thunk, optionally a function of two
; arguments (db handle + thunk) via the #:wrapper keyword.
;
; NOTE:  See call-with-transaction/disconnect below.
;
(define/contract (ensure-disconnect db thnk
                                    #:wrapper [wrapper (lambda (db thnk) (thnk))])
  (->* (connection? (-> any))
       (#:wrapper (-> connection? (-> any) any))
       any)

  (try [(wrapper db thnk)]
       [catch (match-anything refine/raise)]
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
(define/contract (maybe-disconnect db thnk
                                   #:disconnect? disconnect?
                                   #:wrapper [wrapper (lambda (db thnk) (thnk))])
  (->* (connection? (-> any) #:disconnect? boolean?)
       (#:wrapper (-> connection? (-> any) any))
       any)
  ;(say "entering maybe-disconnect.  disconnect is: " disconnect?)
  (cond [disconnect? (ensure-disconnect db thnk #:wrapper wrapper)]
        [else
         (try [(wrapper db thnk)]
              [catch (match-anything refine/raise)])]))

;;    Better name for maybe-disconnect that doesn't need the keyword
(define/contract (disconnect-if should-disconnect? db thnk
                                #:wrapper [wrapper (lambda (db thnk) (thnk))])
  (->* (boolean? connection? (-> any))
       (#:wrapper (-> connection? (-> any) any))
       any)
  ;(say "entering maybe-disconnect.  disconnect is: " disconnect?)
  (cond [should-disconnect? (ensure-disconnect db thnk #:wrapper wrapper)]
        [else
         (try [(wrapper db thnk)]
              [catch (match-anything refine/raise)])]))


;;----------------------------------------------------------------------

;(define/contract (call-with-transaction/disconnect db thnk)
;
; Shorthand for: (ensure-disconnect db thnk call-with-transaction))
(define/contract (call-with-transaction/disconnect db thnk)
  (-> connection? (-> any) any)

  (ensure-disconnect db thnk #:wrapper call-with-transaction))

;;----------------------------------------------------------------------



;; (define sqlite-introspection-sql-hash
;;   (hash
;;    ; List information on all tables in the DB.
;;    ; Returns:  type | name | tbl_name | rootpage | sql |  (NB: sql is the CREATE statement)
;;    'tables @~a{SELECT * FROM sqlite_master WHERE type = 'table'}
;;    ;
;;    ; List all fields in a given table
;;    ; Returns: cid | name | type | notnull | dflt_value | pk |
;;    ; (NB: primary key field(s) have 'pk' field set to 1, rest are set to 0)
;;    'fields (list @~a{SELECT * FROM pragma_table_info($1)} 'param)
;;    ;
;;    ; List all indices in the DB
;;    ; Returns: type | name | tbl_name | rootpage | sql.  sql is the CREATE statement
;;    'indexes  @~a{SELECT * FROM sqlite_master WHERE type = 'index'}
;;    ;
;;    ; List all foreign keys in a specific table.
;;    ; Returns: id | seq | table | from | to | on_update | on_delete | match
;;    ; Example: '(#(0 0 "people" "people_id" #<sql-null> "NO ACTION" "NO ACTION" "NONE")))
;;    'foreign-keys  (λ (table-name) (~a "PRAGMA foreign_key_list(" table-name ")"))
;;    ;
;;    ; List all indexed columns in the DB
;;    ; Returns:  | indexed-columns |
;;    ; Example:  '(#("employees.username") #("people.name")))
;;    'indexed-columns
;;    @~a{
;;        SELECT DISTINCT m.name || '.' || ii.name AS 'indexed-columns'
;;               FROM sqlite_master AS m,
;;               pragma_index_list(m.name) AS il,
;;               pragma_index_info(il.name) AS ii
;;               WHERE m.type='table' ORDER BY 1
;;               }
;;    ))

;; (define (sqlite-introspection-sql sql-statement-name [arg #f])
;;   (define stmt (hash-ref  sqlite-introspection-sql-hash sql-statement-name))
;;   (cond [(procedure? stmt) (stmt arg)]
;;         [else stmt]))

;; (define (sqlite-introspect conn sql-statement-name [arg #f])
;;   (define stmt (sqlite-introspection-sql sql-statement-name arg))
;;   (match stmt
;;     [(list sql 'param) (apply (curry query-rows conn sql) (autobox arg))]
;;     [_ (query-rows conn stmt)]))
