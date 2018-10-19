#!/usr/bin/env racket

#lang at-exp racket/base

(require racket/contract/base
         racket/format
         racket/function
         "../exceptions.rkt"
         "../test-more.rkt")

(ok #t "test harness is working")

(test-suite
 "exn:fail:insufficient-space"

 (lives (thunk (exn:fail:insufficient-space/kw
                #:requested 7
                #:available 8
                #:request-source 'foo))
        "happy path of exn:fail:insufficient-space/kw lives")
 (define exn
   (lives (thunk (exn:fail:insufficient-space/kw
                  #:msg "overruled!"
                  #:requested 7
                  #:available 8
                  #:request-source 'foo))
          "happy path of exn:fail:insufficient-space/kw lives"))
 (like (exn-message exn) #px"overruled!" "successfully overruled message")
 )

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
