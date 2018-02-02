#!/usr/bin/env racket

#lang at-exp racket

(require dstorrs/utils
         dstorrs/json
         dstorrs/test-more
         json
		 )

(ok #t "testing harness works")

(when #t
  (test-suite
   "valid-json?"

   (define valid-json-strings (list @~a{}
                                    #t
                                    @~a{7}
                                    "\"foo\""
                                    @~a{{}}
                                    @~a{[]}
                                    @~a{{"foo": 7}}
                                    @~a{["foo", 7]}))
   (for ((j (flatten (append valid-json-strings
                             (list 7 8.0 -9)))))
     (ok (valid-json? j)
         @~a{json '@j' was valid}))

   (for ((j (list @~a{'f}
                  'f
                  -0.7i
                  identity ; any function, doesn't matter
                  (vector)
                  )))
     (lives (thunk (not-ok (valid-json? j)
                           @~a{correctly saw that this was not valid json: '@j'}))
            @~a{did not die when checking validity of json: @j}))

   ) ; test-suite
  ) ; when

(done-testing)
