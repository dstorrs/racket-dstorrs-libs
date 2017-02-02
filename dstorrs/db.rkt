#lang at-exp rackjure

(require db
		 dstorrs/utils
		 dstorrs/list-utils
         )

;;----------------------------------------------------------------------

(define/contract (placeholders-for lst [start-from 1])
  (->* (list?) (exact-positive-integer?) string?)
  ;;    Create a string that can be used as placeholder values for a
  ;;    group of values suitable for inclusion in a SELECT, INSERT,
  ;;    etc.
  ;; (placeholders-for '(foo bar baz))  => "$1,$2,$3"
  ;; (placeholders-for '())             => ""
  ;; (placeholders-for '(foo bar) 2)    => "$2,$3"  ; start from 2, not 1
  ;;
  (string-join
   (for/list ((i (in-naturals start-from))
              (ignored lst))
     (~a "$" i))
   ","))


;;----------------------------------------------------------------------

(define/contract (placeholders-for-multiple-rows data)
  (-> list? string?)

  (when (null? data)
    (raise-argument-error 'placeholders-for-multiple-rows
                          "data can't be null"
                          data))

  ;(say "data is: " data)

  ;; Turn this:   (("collab1" "desc1") ("collab2" "desc2"))
  ;; Into this:   "($1,$2), ($3,$4)"
  ;;
  ;;    Be permissive if we were given a list instead of a list of
  ;;    lists.
  (let ((vals (if (list? (car data)) data (list data))))
    ;;    (say "vals: " vals)
    (define-values (row-placeholders ignored)
      (for/fold ([lst '()]
                 [placeholder-num 1])
                ((v vals))
        ;; (say "lst: " lst)
        ;; (say "v: "  v)
        ;; (say "pl v: "  (placeholders-for v))
        ;; (say "pl num: " placeholder-num)
        (cons (string-append "(" (placeholders-for v placeholder-num) ")")  lst)

        (values
         (cons (string-append "(" (placeholders-for v placeholder-num) ")")  lst)
         (+ placeholder-num (length v)))
        ))
    ;    (say "row pl: " row-placeholders)
    ;    (say "returning: " (string-join (reverse row-placeholders) ","))

    (string-join (reverse row-placeholders) ",")
    )
  )

;;----------------------------------------------------------------------

;;    Run a query against the database, return the result as a list of
;;    dicts. (By default hashes, but you can override if desired.)
;;    You can also provide a transformer function that will be given
;;    the dict for processing before it is returned.  It must accept a
;;    dict? and return a dict/
;;
;; (query-as-dicts '(chunk-hash chunk-num) (thunk (query-rows db "select hash, chunk_num from chunks")))
;; (query-as-dicts '(chunk-hash chunk-num) (thunk (query-rows db "select hash, chunk_num from chunks")) #:dict-maker hasheq)
;; (query-as-dicts '(chunk-hash chunk-num)
;;                 (thunk (query-rows db "select hash, chunk_num from chunks"))
;;                 #:transform (lambda (d) (hash-set d 'processed #t))) ;; add a key
(define/contract (query-rows-as-dicts keys
                                      db
                                      sql
                                      [vals '()]
                                      #:dict-maker [dict-maker make-hash]
                                      #:transform-dict [transform-dict identity]
                                      #:transform-data [transform-data cons])
  (->* ((non-empty-listof any/c) connection? string?)
       (list?
        #:dict-maker (-> (listof pair?) dict?)   ; takes an assoc list, returns a dict
        #:transform-data (-> any/c any/c pair?)  ; transform the input of dict-maker
        #:transform-dict (-> dict? dict?)        ; transform the output of dict-maker
        )
       (listof dict?))
  ;; (define v->d (curry vector->dict
  ;;                     keys
  ;;                     #:dict-maker dict-maker
  ;;                     #:transform-data transform-data
  ;;                     #:transform-dict transform-dict))
  (define (v->d v)
    (vector->dict
     keys
     v
     #:dict-maker dict-maker
     #:transform-data transform-data
     #:transform-dict transform-dict)
    )
  (map  v->d
        (if (null? vals)
            (query-rows db sql)
            (apply (curry query-rows db sql) vals))))

;;----------------------------------------------------------------------

(define/contract (query-maybe-row-as-dict keys
                                          db
                                          sql
                                          [vals '()]
                                          #:dict-maker [dict-maker make-hash]
                                          #:transform-dict [transform-dict identity]
                                          #:transform-data [transform-data cons])
  (->* ((non-empty-listof any/c) connection? string?)
       (list?
        #:dict-maker (-> (listof pair?) dict?)   ; takes an assoc list, returns a dict
        #:transform-data (-> any/c any/c pair?)  ; transform the input of dict-maker
        #:transform-dict (-> dict? dict?)        ; transform the output of dict-maker
        )
       dict?)

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
         )
