#!/usr/bin/env racket

#lang at-exp racket

(require dstorrs/utils)

(require dstorrs/try
         dstorrs/test-more
         dstorrs/exceptions
         )

(ok #t "test harness is working")

(when #t
  (test-suite
   "refine-db-exn: exn:fail:wrong-number-of-rows"

   (define wrong-rows (refine-db-exn (create-exn exn:fail
                                                 @~a{query-value: query returned wrong number of rows
                                                                  statement: "SELECT id FROM users WHERE username = $1"
                                                                  expected: 1
                                                                  got: 0})))
   (is-type wrong-rows
            exn:fail:wrong-number-of-rows?
            "got an exn:fail:wrong-number-of-rows")

   (is (exn:fail:wrong-number-of-rows-expected wrong-rows)
       1
       "it expected 1 row")

   (is (exn:fail:wrong-number-of-rows-got wrong-rows)
       0
       "correctly said that it got 0 rows")
   ) ; test-suite
  ) ; when

(displayln "Done testing.")
