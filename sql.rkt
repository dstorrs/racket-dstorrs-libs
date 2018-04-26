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
;;  "X_to_Y" where X and Y are alphabetical.
;;
;;  (join-table-name "users" "public_keys") => "public_keys_to_users"
;;  (join-table-name "public_keys" "users") => "public_keys_to_users"
(define/contract (join-table-name table1 table2)
  (-> string? string? string?)
  (string-join (sort (list table1 table2)
                     string-ci<?)
               "_to_"))

;;--------------------------------------------------------------------------------

;;   Produces the abbrevation for a join table, which will be of the
;;   format "x2y" See join-table-name above for how to get the name.
;;   Names are always in alphabetical order and so are abbreviations:
;;
;;    (join-table-abbrev "endpoints" "users") => e2u
;;    (join-table-abbrev "users" "endpoints") => e2u
;;
(define/contract (join-table-abbrev t1 t2)
  (-> string? string? string?)
  (define-values (table1 table2) (apply values (sort (list t1 t2) string-ci<?)))
  (~a (string-ref table1 0) "2" (string-ref table2 0)))

;;--------------------------------------------------------------------------------


;; (define/contract (many-to-many-join table1 join-to)
;;  (-> string? (or/c string? (non-empty-listof string?)) string?)
;;
;;    ; Join two tables together
;; (many-to-many-join "collaborations" "files")
;; Return: "collaborations c JOIN collaborations_to_files c2f ON c.id = c2f.collaboration_id JOIN files f ON c2f.file_id = f.id"
;;
;;    ; Join three tables by way of a central table.
;; (many-to-many-join "users" '("endpoints" "collaborations"))
;; Return: "users u JOIN endpoints_to_users e2u on e2u.user_id = u.id JOIN endpoints e ON e2u.endpoint_id = e.id JOIN collaborations_to_users c2u ON c2u.user_id = u.id JOIN collaborations c ON c2u.collaboration_id = c.id"
;;
;;   NOTE: You *must* join through a central table. Joining from one
;;   end of the chain does not work, since there is not a direct connection. 
;; (many-to-many-join "endpoints" '("users" "collaborations")) ; RESULT INVALID
;; Return: "endpoints e JOIN endpoints_to_users e2u ON e2u.endpoint_id = e.id JOIN users u ON e2u.user_id = u.id JOIN collaborations_to_endpoints c2e ON c2e.endpoint_id = e.id JOIN collaborations c ON c2e.collaboration_id = c.id"
;;    THE ABOVE SQL DOES NOT WORK. THERE IS NO collaborations_to_endpoints TABLE
;;
(define/contract (many-to-many-join table1 join-to)
  (-> string? (or/c string? (non-empty-listof string?)) string?)

  ;  NB: string-ref returns a char and we want a string, hence the ~a
  (define t1a         (~a (string-ref table1 0)))          ; e.g. collaborations => c

  (cond [(list? join-to)
         (define lst (for/list ([to join-to])
                       (many-to-many-join table1 to)))
         (string-join (cons (car lst)
                            (map (lambda (s)
                                   (string-trim s @~a{@table1 @t1a }))
                                 (cdr lst))))]
        [else
         (define (singular str)
           (define res (regexp-match #px"^(.+)s$" str))
           (if res (second res) str))

         ; If given, e.g., (many-to-many-join "files" "collaborations")
         ;
         ;(define table1     "files")                              ; argument to func
         ;(define join-to    "collaborations")                     ; argument to func
         ;(define t1a        (string-ref table1 0))                ; f  [defined above]
         (define t2a         (~a (string-ref join-to 0)))          ; c
         (define join-table  (join-table-name table1 join-to))     ; collaborations_to_files
         (define jta         (join-table-abbrev table1 join-to))   ; c2f
         (define t1-id       (~a t1a ".id"))                       ; f.id
         (define t2-id       (~a t2a ".id"))                       ; c.id
         (define t1-link     (~a jta "." (singular table1) "_id")) ; c2f.file_id
         (define t2-link     (~a jta "." (singular join-to) "_id")); c2f.collaboration_id

         @~a{@table1 @t1a JOIN @join-table @jta ON @t1-id = @t1-link JOIN @join-to @t2a ON @t2-link = @t2-id}
         ]))

;;--------------------------------------------------------------------------------
