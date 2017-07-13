#!/usr/bin/env racket

#lang at-exp racket

(require dstorrs/utils
         dstorrs/try
         dstorrs/test-more
         dstorrs/exceptions
         )

(ok #t "test harness is working")

(when #t
  (test-suite
   "verify-arg"

   (lives (thunk
           (verify-arg "requested-space" 7 exact-positive-integer? 'has-sufficient-space)
           )
          @~a{Lives: (verify-arg "requested-space" 7 exact-positive-integer? 'has-sufficient-space)})

   (lives (thunk
           (verify-arg "next-piece" 'apple (or/c 'apple 'pear) 'check-fruit-type))
          @~a{Lives: (verify-arg "next-piece" 'apple (or/c 'apple 'pear) 'check-fruit-type)}
          )

   (throws (thunk 
            (verify-arg "requested-space" 'bob exact-positive-integer? 'has-sufficient-space))
           @pregexp{has-sufficient-space:\s+'requested-space' must be exact-positive-integer\?\s+requested-space:\s+'bob}   
           @~a{throws expected message: (verify-arg "requested-space" 'bob exact-positive-integer? 'has-sufficient-space)})
   ;; )

   (throws (thunk
            (verify-arg "next-piece" 'banana (or/c 'apple 'pear) 'check-fruit-type)
            )
           @pregexp{check-fruit-type:\s+'next-piece' must be flat-or/c\s+next-piece:\s+'banana}
           @~a{throws expected message: (verify-arg "next-piece" 'banana (or/c 'apple 'pear) 'check-fruit-type)}
           )

   (throws (thunk
            (verify-arg "next-piece" 'banana (or/c 'apple 'pear) 'check-fruit-type "apple or pear")
            )
           @pregexp{check-fruit-type:\s+'next-piece' must be apple or pear\s+next-piece:\s+'banana}
           @~a{throws expected message: (verify-arg "next-piece" 'banana (or/c 'apple 'pear) 'check-fruit-type "apple or pear")}
           )
   )
  )

(done-testing)
