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
   "refine-db-exn: exn:fail:db:num-rows"

   (define wrong-rows-0 (refine-db-exn (create-exn exn:fail
                                                 @~a{query-value: query returned wrong number of rows
                                                                  statement: "SELECT id FROM users WHERE username = $1"
                                                                  expected: 1
                                                                  got: 0})))
   (is-type wrong-rows-0
            exn:fail:db:num-rows?
            "wrong-rows-0 is an exn:fail:db:num-rows")

   (is-type wrong-rows-0
            exn:fail:db:num-rows:zero?
            "wrong-rows-0 is also an exn:fail:db:num-rows:zero")

   (is (exn:fail:db:num-rows-expected wrong-rows-0)
       1
       "it expected 1 row")

   (is (exn:fail:db:num-rows-got wrong-rows-0)
       0
       "correctly said that it got 0 rows")


   ;;--------------------
   
   (define wrong-rows-2 (refine-db-exn (create-exn exn:fail
                                                 @~a{query-value: query returned wrong number of rows
                                                                  statement: "SELECT id FROM users WHERE username = $1"
                                                                  expected: 1
                                                                  got: 2})))
   (is-type wrong-rows-2
            exn:fail:db:num-rows?
            "wrong-rows-2 is an exn:fail:db:num-rows")

   (is-type wrong-rows-2
            exn:fail:db:num-rows:many?
            "wrong-rows-2 is also an exn:fail:db:num-rows:many")

   (is (exn:fail:db:num-rows-expected wrong-rows-2)
       1
       "it expected 1 row")

   (is (exn:fail:db:num-rows-got wrong-rows-2)
       2
       "correctly said that it got 0 rows")


   
   ) ; test-suite
  ) ; when

(displayln "Done testing.")
