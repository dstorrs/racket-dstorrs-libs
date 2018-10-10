#lang at-exp racket/base

(require racket/contract/base
         racket/contract/region
         racket/format
         racket/function
         racket/list
         racket/string)

(provide (all-defined-out))

;;(define/contract (placeholders-for lst [start-from 1] #:for-update? [for-update? #f])
;;  (->* (list?) (exact-positive-integer? #:for-update? boolean?) string?)
;;
;;    Create a string that can be used as placeholder values for a
;;    group of values suitable for inclusion in a SELECT, INSERT, or
;;    UPDATE statement.
;;
;; (placeholders-for '(foo bar baz))  => "$1,$2,$3"
;; (placeholders-for '())             => ""
;; (placeholders-for '(foo bar) 2)    => "$2,$3"  ; start from 2, not 1
;;
;; (placeholders-for '(foo bar) 3 #:for-update? #t) => "foo=$3,bar=$4" ; sql UPDATE form
;;
;; NB: If for-update? is #f then it doesn't matter what's in the list
;; you pass, only the length of the list.  If it's #t then you MUST
;; pass the names of the columns.
;;
(define/contract (placeholders-for lst [start-from 1] #:for-update? [for-update? #f])
  (->* (list?) (exact-positive-integer? #:for-update? boolean?) string?)
  (string-join
   (for/list ((i (in-naturals start-from))
              (key lst))
     (cond [for-update? (format "~a=$~a" key i)]
           [else        (format "$~a" i)]))
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

;; (many-to-many-join table1 join-to #:left? [left? #f] #:skip-first? [skip-first? #f])
;;   (->* (string? (or/c string? (non-empty-listof string?)))
;;        (#:left? boolean? #:skip-first? boolean)
;;        string?)
;;
;;  ASSUMPTIONS:
;;    This function is opinionated.  It assumes that:
;;        * tables are named as plurals ending in 's' (e.g. 'collaborations', 'users')
;;        * join tables are named in the style X_to_Y in alphabetical order (e.g. 'collaborations_to_users')
;;        * all id fields are 'id' (e.g. 'collaborations.id')
;;        * join table fields are of the form collaboration_id, user_id, etc
;;
;;
;;   See 'many-to-many-natural-join' below for a different set of assumptions
;;
;;
;;    ; Join two tables together
;; (many-to-many-join "collaborations" "files")
;; Return: "collaborations c JOIN collaborations_to_files c2f ON c.id = c2f.collaboration_id JOIN files f ON c2f.file_id = f.id"
;;
;;    ; Join two tables together with left joins
;; (many-to-many-join "collaborations" "files" #:left? #t)
;; Return: "collaborations c LEFT JOIN collaborations_to_files c2f ON c.id = c2f.collaboration_id LEFT JOIN files f ON c2f.file_id = f.id"
;;
;;    ; Join two tables together but don't mention the first one
;;    ; (presumably because it was already joined earlier in the SQL
;;    ; that you're building)
;; (many-to-many-join "collaborations" "files" #:skip-first? #t)
;; Return: "JOIN collaborations_to_files c2f ON c.id = c2f.collaboration_id JOIN files f ON c2f.file_id = f.id"
;;
;;    ; Combine the functionality of the above two
;; (many-to-many-join "collaborations" "files" #:skip-first? #t #:left? #t)
;; Return: "LEFT JOIN collaborations_to_files c2f ON c.id = c2f.collaboration_id LEFT JOIN files f ON c2f.file_id = f.id"
;;
;;    ; Join three tables by way of a central table.
;;    ; i.e.  users is m2m with endpoints and m2m with collaborations
;;
;; (many-to-many-join "users" '("endpoints" "collaborations"))
;; Return: "users u JOIN endpoints_to_users e2u on e2u.user_id = u.id JOIN endpoints e ON e2u.endpoint_id = e.id JOIN collaborations_to_users c2u ON c2u.user_id = u.id JOIN collaborations c ON c2u.collaboration_id = c.id"
;;
;;   NOTE: You *must* join through a central table. Joining from one
;;   end of the chain does not work, since there is not a direct
;;   connection between the tables on the ends.  In this example,
;;   there is no collaborations_to_endpoints table.  The function has
;;   no way of knowing that, so it will return the appropriate SQL
;;   string, it's just that the string won't work in your DB.
;;
;; (many-to-many-join "endpoints" '("users" "collaborations")) ; RESULT INVALID
;; Return: "endpoints e JOIN endpoints_to_users e2u ON e2u.endpoint_id = e.id JOIN users u ON e2u.user_id = u.id JOIN collaborations_to_endpoints c2e ON c2e.endpoint_id = e.id JOIN collaborations c ON c2e.collaboration_id = c.id"
;;
;;    THE ABOVE SQL DOES NOT WORK if there is no collaborations_to_endpoints table
;;
(define/contract (many-to-many-join table1 join-to #:left? [left? #f] #:skip-first? [skip-first? #f])
  (->* (string? (or/c string? (non-empty-listof string?)))
       (#:left? boolean? #:skip-first? boolean?)
       string?)

  ;  NB: string-ref returns a char and we want a string, hence the ~a
  (define t1a (~a (string-ref table1 0))) ; e.g. collaborations => c

  (cond [(list? join-to)
         (define lst (for/list ([to join-to])
                       (many-to-many-join table1 to #:left? left? #:skip-first? skip-first?)))
         (define result 
           (string-join (cons (car lst)
                            (map (lambda (s)
                                   (string-trim s @~a{@table1 @t1a }))
                                 (cdr lst)))))
         (cond [skip-first? (string-trim result @~a{@table1 @t1a })]
               [else result])]
        [else
         (define (singular str)
           (define res (regexp-match #px"^(.+)s$" str))
           (if res (second res) str))

         ; If given, e.g., (many-to-many-join "files" "collaborations")
         ;
         ;(define table1     "files")                             ; argument to func
         ;(define join-to    "collaborations")                    ; argument to func
         ;(define t1a        (string-ref table1 0))               ; f  [defined above]
         (define t2a         (~a (string-ref join-to 0)))         ; c
         (define join-table  (join-table-name table1 join-to))    ; collaborations_to_files
         (define jta         (join-table-abbrev table1 join-to))  ; c2f
         (define t1-id-field (~a (singular table1) "_id"))        ; file_id
         (define t2-id-field (~a (singular join-to) "_id"))       ; collaboration_id
         (define t1-id       (~a t1a ".id")); f.id  ; t1-id-field))            ; f.file_id
         (define t2-id       (~a t2a ".id")); c.id  ; t2-id-field))            ; c.file_id
         (define t1-link     (~a jta "." t1-id-field))            ; c2f.file_id
         (define t2-link     (~a jta "." t2-id-field))            ; c2f.collaboration_id
         (define join-type   (if left? "LEFT JOIN" "JOIN"))
         
         (define start @~a{@table1 @t1a })
         (~a (if skip-first?
                 ""
                 start) 
             @~a{@join-type @join-table @jta ON @t1-id = @t1-link @join-type @join-to @t2a ON @t2-link = @t2-id})
         ]))

;;--------------------------------------------------------------------------------

;; (define/contract (many-to-many-join table1 join-to)
;;   (->* (string? (or/c string? (non-empty-listof string?)))
;;        (#:left? boolean?)
;;        string?)
;;
;;  ASSUMPTIONS:
;;    This function is opinionated.  It assumes that:
;;        * tables are named as plurals ending in 's' (e.g. 'collaborations', endpoints')
;;        * join tables are named in the style X_to_Y in alphabetical order (e.g. 'collaborations_to_endpoints')
;;        * all id fields are of the form '<singular_table_name>_id' (e.g. 'collaboration_id')
;;        * join table fields follow the same convention to allow for NATURAL JOIN
;;
;;
;;    ; Join two tables together
;; (many-to-many-join "collaborations" "files")
;; Return: "collaborations c NATURAL JOIN collaborations_to_files c2f NATURAL JOIN files f"
;;
;;    ; Order doesn't matter
;; (many-to-many-join "files" "collaborations")
;; Return: "collaborations c NATURAL JOIN collaborations_to_files c2f NATURAL JOIN files f"
;;
;;    ; Join three tables by way of a central table
;; (many-to-many-join "users" '("endpoints" "collaborations"))
;; Return: "users u NATURAL JOIN endpoints_to_users e2u NATURAL JOIN endpoints e NATURAL JOIN collaborations_to_users c2u NATURAL JOIN collaborations c"
;;
;;   NOTE: You *must* join through a central table. Joining from one
;;   end of the chain does not work, since there is not a direct connection. 
;; (many-to-many-join "endpoints" '("users" "collaborations")) ; RESULT INVALID
;; Return: "endpoints e NATURAL JOIN endpoints_to_users e2u NATURAL JOIN users u NATURAL JOIN collaborations_to_endpoints c2e NATURAL JOIN collaborations c"
;;    THE ABOVE SQL DOES NOT WORK if there is no collaborations_to_endpoints table
;;
;;   You can use the optional '#:left?' param to make it use 'NATURAL LEFT JOIN' on all links
;;
(define/contract (many-to-many-natural-join table1 join-to #:left? [left? #f])
  (->* (string? (or/c string? (non-empty-listof string?)))
       (#:left? boolean?)
       string?)

  ;  NB: string-ref returns a char and we want a string, hence the ~a
  (define t1a         (~a (string-ref table1 0)))          ; e.g. collaborations => c

  (cond [(list? join-to)
         (define lst (for/list ([to join-to])
                       (many-to-many-natural-join table1 to)))
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
         (define join-type   (if left? "NATURAL LEFT JOIN" "NATURAL JOIN"))
         
         @~a{@table1 @t1a @join-type @join-table @jta @join-type @join-to @t2a}
         ]))

;;--------------------------------------------------------------------------------
