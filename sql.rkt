#lang at-exp racket

(provide (all-defined-out))


;;(define/contract (placeholders-for lst [start-from 1])
;;
;;    Create a string that can be used as placeholder values for a
;;    group of values suitable for inclusion in a SELECT, INSERT,
;;    etc.
;; (placeholders-for '(foo bar baz))  => "$1,$2,$3"
;; (placeholders-for '())             => ""
;; (placeholders-for '(foo bar) 2)    => "$2,$3"  ; start from 2, not 1
;;
(define/contract (placeholders-for lst [start-from 1])
  (->* (list?) (exact-positive-integer?) string?)
  (string-join
   (for/list ((i (in-naturals start-from))
              (ignored lst))
     (~a "$" i))
   ","))

;;--------------------------------------------------------------------------------

;; (define/contract (placeholders-for-multiple-rows data [start-from 1])
;;
;; Same as placeholders-for but takes a LoL and builds a string
;; suitable for inserting multiple rows.  Example:
;;
;; (placeholders-for-multiple-rows '(("foo") ("bar")))   => "($1),($2)"
;; (placeholders-for-multiple-rows '(("foo") ("bar") 4)) => "($4),($5)"
(define/contract (placeholders-for-multiple-rows data [start-from 1])
  (->* ((or/c (non-empty-listof (not/c list?))
              (non-empty-listof (non-empty-listof (not/c list?)))))
       (exact-positive-integer?)
       string?)

  (when (null? data)
    (raise-argument-error 'placeholders-for-multiple-rows
                          "data can't be null"
                          data))

  (define first-element (first data))
  (cond [(not (list? first-element)) 'do-nothing]
        [else
         (define expected-length (length first-element))
         (unless (andmap (compose (curry equal? expected-length)
                                  length)
                         data)
           (raise-argument-error 'placeholders-for-multiple-rows
                                 "list of equal-length lists"
                                 data))])


  ;; Turn this:   (("collab1" "desc1") ("collab2" "desc2"))
  ;; Into this:   "($1,$2), ($3,$4)"
  ;;
  ;;    Be permissive if we were given a list instead of a list of
  ;;    lists.
  (let ((vals (if (list? first-element)
                  data
                  (list data))))
    ;;    (say "vals: " vals)
    (define-values (row-placeholders ignored)
      (for/fold ([lst '()]
                 [placeholder-num start-from])
                ((v vals))
        (cons (string-append "(" (placeholders-for v placeholder-num) ")")  lst)

        (values
         (cons (string-append "(" (placeholders-for v placeholder-num) ")")  lst)
         (+ placeholder-num (length v)))
        ))

    (string-join (reverse row-placeholders) ",")
    )
  )

;;----------------------------------------------------------------------

;; (define/contract (sql-IN-clause lst [start-from 1])
;;
;; Generates an IN clause of appropriate length for its args.  Doesn't
;; care what its args are, just how many there are.
;;
;;    (sql-IN-clause '(a b c))   => "IN ($1,$2,$3)"
;;    (sql-IN-clause '(a b c) 4) => "IN ($4,$5,$6)"
(define/contract (sql-IN-clause lst [start-from 1])
  (->* ((or/c (non-empty-listof (not/c list?))
              (non-empty-listof (non-empty-listof (not/c list?)))))
       (natural-number/c)
       string?)
  (string-append "IN ("
                 ((if (list? (first lst)) placeholders-for-multiple-rows placeholders-for)
                  lst
                  start-from)
                 ")"))

;;--------------------------------------------------------------------------------

;; (define/contract (join-table-name table1 table2)
;;
;;  Takes two strings, each the name of a table, and generates the
;;  name of the join table, assuming that the join table is named
;;  "X_to_Y".
;;
;;  (join-table-name "users" "public_keys") => "users_to_public_keys"
(define/contract (join-table-name table1 table2)
  (-> string? string? string?)
  (string-append table1 "_to_" table2))

;;--------------------------------------------------------------------------------


;; (define/contract (many-to-many-join table1 join-to)
;;  (-> string? (or/c string? (non-empty-listof string?)) string?)
;;
;;    ; Join two tables together
;; (many-to-many-join "collaborations" "files")
;; Return: "collaborations c JOIN collaborations_to_files c2f ON c.id = c2f.collaboration_id JOIN files f ON c2f.file_id = f.id"
;;
;;    ; Join three tables by way of a central table
;; (many-to-many-join "collaborations" '("files" "endpoints"))
;; Return: "collaborations c JOIN collaborations_to_files c2f ON c.id = c2f.collaboration_id JOIN files f ON c2f.file_id = f.id JOIN collaborations_to_endpoints c2e ON c.id = c2e.collaboration_id JOIN endpoints e ON c2e.endpoint_id = e.id"
;;
(require handy/utils)
(define/contract (many-to-many-join table1 join-to)
  (-> string? (or/c string? (non-empty-listof string?)) string?)

  (define t1a         (~a (string-ref table1 0)))          ; collaborations => c
  
  (cond [(list? join-to)
         (say "join-to: " (~v join-to))
         (define lst (for/list ([to join-to])
                       (many-to-many-join table1 to)))
         (say "lst is: " (~v lst))
         (string-join (cons (car lst)
                            (map (lambda (s)
                                   (string-trim s @~a{@table1 @t1a }))
                                 (cdr lst))))]
        [else
         (define (singular str)
           (define res (regexp-match #px"^(.+)s$" str))
           (if res (second res) str))

         (define join-table  (join-table-name table1 join-to))     ; collaborations_to_files
         (define t2a         (~a (string-ref join-to 0)))          ; f
         (define jta         (~a t1a "2" t2a))                    ; c2f
         (define t1-id       (~a t1a ".id"))                      ; c.id
         (define t2-id       (~a t2a ".id"))                      ; f.id
         (define t1-link     (~a jta "." (singular table1) "_id")); c2f.collaboration_id
         (define t2-link     (~a jta "." (singular join-to) "_id")); c2f.file_id

         ;; collaborations c JOIN collaborations_to_files c2f ON c.id = c2f.collaboration_id JOIN files f ON c2f.file_id = f.id
         ;; collaborations c JOIN collaborations_to_endpoints c2e ON c.id = c2e.collaboration_id JOIN endpoints e ON c2e.endpoint_id = e.id
         @~a{@table1 @t1a JOIN @join-table @jta ON @t1-id = @t1-link JOIN @join-to @t2a ON @t2-link = @t2-id}
         ]))

;;--------------------------------------------------------------------------------
