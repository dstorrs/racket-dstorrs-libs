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

 (is (many-to-many-join c f)
     "collaborations c JOIN collaborations_to_files c2f ON c.id = c2f.collaboration_id JOIN files f ON c2f.file_id = f.id"
     "many-to-many-join works")

 (is (many-to-many-join "collaborations" '("files" "endpoints"))
     "collaborations c JOIN collaborations_to_files c2f ON c.id = c2f.collaboration_id JOIN files f ON c2f.file_id = f.id JOIN collaborations_to_endpoints c2e ON c.id = c2e.collaboration_id JOIN endpoints e ON c2e.endpoint_id = e.id"
     "(many-to-many-join \"collaborations\" '(\"files\" \"endpoints\")) works")

 (is (many-to-many-join "collaborations" '("files" "endpoints" "users"))
     "collaborations c JOIN collaborations_to_files c2f ON c.id = c2f.collaboration_id JOIN files f ON c2f.file_id = f.id JOIN collaborations_to_endpoints c2e ON c.id = c2e.collaboration_id JOIN endpoints e ON c2e.endpoint_id = e.id JOIN collaborations_to_users c2u ON c.id = c2u.collaboration_id JOIN users u ON c2u.user_id = u.id"
     "(many-to-many-join \"collaborations\" '(\"files\" \"endpoints\" \"users\")) works")
 )

(when #t
  (test-suite
   "sql-IN-clause"

   (is (sql-IN-clause '(foo bar))
       "IN ($1,$2)"
       "correct: (sql-IN-clause '(foo bar))")

   (is (sql-IN-clause '(foo bar) 3)
       "IN ($3,$4)"
       "correct: (sql-IN-clause '(foo bar) 3)")

   (is (sql-IN-clause '((foo bar)(baz jaz)))
       "IN (($1,$2),($3,$4))"
       "correct:  (sql-IN-clause '((foo bar)(baz jaz)))")

   (is (sql-IN-clause '((foo bar)(baz jaz)) 1)
       "IN (($1,$2),($3,$4))"
       "correct:  (sql-IN-clause '((foo bar)(baz jaz)))")

   (is (sql-IN-clause '((foo bar)(baz jaz)) 3)
       "IN (($3,$4),($5,$6))"
       "correct:  (sql-IN-clause '((foo bar)(baz jaz)) 3)")

   (throws (thunk (sql-IN-clause '((foo bar)(baz jaz)) 0))
           exn:fail:contract?
           "start-from must be >= 1")

   (throws (thunk (sql-IN-clause '((foo bar)(baz)) 1))
           #px"list of equal-length lists"
           "when setting up for multiple rows, all records must be the same length")

   (throws (thunk (sql-IN-clause '(()) 1))
           exn:fail:contract?
           "can't have empty list in the data list")

   );test-suite
  );when

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

   (is (placeholders-for-multiple-rows '((a b c) (d e f)) 3)
       "($3,$4,$5),($6,$7,$8)"
       "correctly built multiple-rows placeholders with start-from 3")

   (is (placeholders-for-multiple-rows '(a b c) 3)
       "($3,$4,$5)"
       "correctly built one-row placeholders from list with start-from 3")

   (is (placeholders-for-multiple-rows '((a b c)) 3)
       "($3,$4,$5)"
       "correctly built one-row placeholders from LoL with start-from 3")
   )
  )

(done-testing)
