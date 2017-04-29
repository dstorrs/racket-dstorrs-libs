#lang at-exp racket

(provide (all-defined-out))

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

;;--------------------------------------------------------------------------------

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

(define/contract (sql-IN-clause lst)
  (-> list? string?)
  (string-append "IN ("
                 (placeholders-for lst)
                 ")"))

;;--------------------------------------------------------------------------------

(define/contract (join-table-name table1 table2)
  (-> string? string? string?)
  ;; given:   collaborations files
  ;; returns: collaborations_to_files
  (string-append table1 "_to_" table2))

;;--------------------------------------------------------------------------------

(define/contract (join-clause table1 table2)
  (-> string? string? string?)
  ;; Given:  "collaborations" "files"
  ;; Return: "collaborations c JOIN collaborations_to_files c2f ON c.id = c2f.collaboration_id JOIN files f ON c2f.file_id = f.id"

  (define (singular str) (second (regexp-match #px"^(.+)s$" str)))

  (define join-table  (join-table-name table1 table2))     ; collaborations_to_files
  (define t1a         (~a (string-ref table1 0)))          ; c
  (define t2a         (~a (string-ref table2 0)))          ; f
  (define jta         (~a t1a "2" t2a))                    ; c2f
  (define t1-id       (~a t1a ".id"))                      ; c.id
  (define t2-id       (~a t2a ".id"))                      ; f.id
  (define t1-link     (~a jta "." (singular table1) "_id")); c2f.collaboration_id
  (define t2-link     (~a jta "." (singular table2) "_id")); c2f.file_id

  ;; collaborations c JOIN collaborations_to_files c2f ON c.id = c2f.collaboration_id JOIN files f ON c2f.file_id = f.id
    
  @~a{@table1 @t1a JOIN @join-table @jta ON @t1-id = @t1-link JOIN @table2 @t2a ON @t2-link = @t2-id}
  )

;;--------------------------------------------------------------------------------

(define/contract (clause-convert-epoch->timestamp [param-num 1] #:subquery [sub #f])
  (->* () (natural-number/c #:subquery boolean?) string?)
  @~a{@(if sub "(" "")SELECT timestamp 'epoch' + INTERVAL '1 second' * $@|param-num|@(if sub ")" "")})

;;--------------------------------------------------------------------------------

