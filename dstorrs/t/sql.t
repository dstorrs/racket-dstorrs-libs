#!/usr/bin/env racket

#lang at-exp racket

(require "../test-more.rkt"
         "../sql.rkt"
         )

(void (ok #t "test harness is working"))

(test-suite
 "join-related functions"

 (define c "collaborations")
 (define f "files")

 (is (join-table-name c f)
     "collaborations_to_files"
     "join-table-name works")

 (is (join-clause c f)
     "collaborations c JOIN collaborations_to_files c2f ON c.id = c2f.collaboration_id JOIN files f ON c2f.file_id = f.id"
     "join-clause works")
 )

(test-suite
 "date- and timestamp-related functions"
 (is (clause-convert-epoch->timestamp)
     "timestamp 'epoch' + INTERVAL '1 second' * $1"
     "clause-convert-epoch->timestamp: got correct string back for no params")

 (is (clause-convert-epoch->timestamp #:complete #t)
     "SELECT timestamp 'epoch' + INTERVAL '1 second' * $1"
     "clause-convert-epoch->timestamp: got correct string back for no params with #:complete")

 (is (clause-convert-epoch->timestamp 3)
     "timestamp 'epoch' + INTERVAL '1 second' * $3"
     "clause-convert-epoch->timestamp: got correct string back for one param")

 (is (clause-convert-epoch->timestamp 3 #:complete #t)
     "SELECT timestamp 'epoch' + INTERVAL '1 second' * $3"
     "clause-convert-epoch->timestamp: got correct string back for one param with #:complete")

 (is (clause-convert-epoch->timestamp #:subquery #t)
     "(SELECT timestamp 'epoch' + INTERVAL '1 second' * $1)"
     "clause-convert-epoch->timestamp: got correct string back for subquery")

 (is (clause-convert-epoch->timestamp #:subquery #t #:complete #f)
     "(SELECT timestamp 'epoch' + INTERVAL '1 second' * $1)"
     "clause-convert-epoch->timestamp: got correct string back for subquery with #:complete #f")

 (is (clause-convert-timestamp->epoch)
     "extract('epoch' from $1)"
     "clause-convert-timestamp->epoch: got correct string back for no param")

 (is (clause-convert-timestamp->epoch #:complete #t)
     "SELECT extract('epoch' from $1)"
     "clause-convert-timestamp->epoch: got correct string back for no param with #:complete")

 (is (clause-convert-timestamp->epoch 3)
     "extract('epoch' from $3)"
     "clause-convert-timestamp->epoch: got correct string back for integer param")

 (is (clause-convert-timestamp->epoch 3 #:complete #t)
     "SELECT extract('epoch' from $3)"
     "clause-convert-timestamp->epoch: got correct string back for integer param with #:complete")

 (is (clause-convert-timestamp->epoch (clause-convert-epoch->timestamp #:subquery #t))
     "extract('epoch' from (SELECT timestamp 'epoch' + INTERVAL '1 second' * $1))"
     "clause-convert-timestamp->epoch: got correct string back for string param")

 (is (clause-convert-timestamp->epoch #:complete #t (clause-convert-epoch->timestamp #:subquery #t))
     "SELECT extract('epoch' from (SELECT timestamp 'epoch' + INTERVAL '1 second' * $1))"
     "clause-convert-timestamp->epoch: got correct string back for string param with #:complete")
 )

(test-suite
 "sql-IN-clause"

 (is (sql-IN-clause '(foo bar))
     "IN ($1,$2)"
     "correct: (sql-IN-clause '(foo bar))")

 (is (sql-IN-clause '(foo bar) 3)
     "IN ($3,$4)"
     "correct: (sql-IN-clause '(foo bar) 3)")
 )

(test-suite
 "var->column"
 (is (var->column "foo")
     "foo"
     "var->column \"foo\" works")

 (is (var->column 'foo)
     "foo"
     "var->column 'foo works")

 (is (var->column "foo-bar")
     "foo_bar"
     "var->column \"foo-bar\" works")

 (is (var->column 'foo-bar)
     "foo_bar"
     "var->column 'foo-bar works")

 (is (var->column "FOO-BAR")
     "foo_bar"
     "var->column \"FOO-BAR\" works")

 (is (var->column "FOO-BAR-BAX")
     "foo_bar_bax"
     "var->column \"FOO-BAR-BAX\" works")

 (throws (thunk (var->column ""))
         #px"expected:\\s+\\(or/c\\s+non-empty-string\\?\\s+symbol\\?\\)"
         "var->column throws on empty string")
 )

(test-suite
 "var-list->column-clause"
 (is (var-list->column-clause (list "foo" "bar"))
     "foo,bar"
     "var-list->column-clause '(\"foo\" \"bar\" works")

 (is (var-list->column-clause '(foo "bar"))
     "foo,bar"
     "var-list->column-clause '(foo \"bar\" works")

 (is (var-list->column-clause 'foo "bar")
     "foo,bar"
     "var-list->column-clause '(foo \"bar\" works")

 (is (var-list->column-clause '("foo-bar"))
     "foo_bar"
     "var-list->column-clause '(\"foo-bar\") works")

 (is (var-list->column-clause 'foo-bar)
     "foo_bar"
     "var-list->column-clause 'foo-bar works")

 (is (var-list->column-clause "FOO-BAR" 'BAZ-quux)
     "foo_bar,baz_quux"
     "var-list->column-clause \"FOO-BAR\" 'BAZ-quux works")

 (is (var-list->column-clause "FOO-BAR-BAX" "glop,blig")
     "foo_bar_bax,glop,blig"
     "var-list->column-clause \"FOO-BAR-BAX\" \"glop,blig\" works")

 ;; (is (var-list->column-clause)
 ;;     ""
 ;;     "var-list->column-clause returns empty string")

 (is (var-list->column-clause '())
     ""
     "var-list->column-clause '() returns empty string")
 )

(when #t
  (test-suite
   "placeholders-for"

   (for ((args (list '() '(foo) '(foo bar) '(foo bar 3) '(foo bar (1 2))))
         (res  (list "" "$1" "$1,$2" "$1,$2,$3" "$1,$2,$3" )))
     (is (placeholders-for args) res @~a{(placeholders-for @args) is @res})
     )
   (is (placeholders-for '(foo bar 3) 3)
       "$3,$4,$5"
       @~a{(placeholders-for '(foo bar 3) 3) is "$3,$4,$5"})
   )
  )

(when #t
  (test-suite
   "placeholders-for-multiple-rows"

   (is (placeholders-for-multiple-rows '((a b c) (d e f)))
       "($1,$2,$3),($4,$5,$6)"
       "correctly built multiple-rows placeholders")

   (is (placeholders-for-multiple-rows '(a b c))
       "($1,$2,$3)"
       "correctly built one-row placeholders from list")

   (is (placeholders-for-multiple-rows '((a b c)))
       "($1,$2,$3)"
       "correctly built one-row placeholders from LoL")
   )
  )

(done-testing)
