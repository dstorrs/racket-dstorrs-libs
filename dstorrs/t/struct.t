#!/usr/bin/env racket

#lang at-exp rackjure

(require "../struct.rkt"
         "../test-more.rkt"
         )

(void (ok 1 "test harness is working"))

(test-suite
 "struct/kw"

 ;; Define a struct type
 (struct/kw foo (a b [c 42]) #:transparent)
 (define (checker thnk description #:a [a-val 1] #:b [b-val 2] #:c [c-val 3])
   (match (thnk)
     [(struct* foo ([a a] [b b] [c c]))
      (begin
        (is a a-val (~a description " => got correct a"))
        (is b b-val (~a description " => got correct b"))
        (is c c-val (~a description " => got correct c")))]
     [_ (ok #f (~a description " => did not match"))])
   )
 ;;
 ;; Use normal ctor
 (checker (thunk (foo 1 2 3)) "(foo 1 2 3)")
  ;;
  ;; Use keyword ctor
 (checker (thunk (foo/kw #:a 1 #:b 2 #:c 3))
          "(foo/kw #:a 1 #:b 2 #:c 3)") ; => (foo 1 2 3)
  ;;
 ;; Use keyword ctor, taking advantage of default arg for #:c field
 (checker (thunk (foo/kw #:a 1 #:b 2))       ; => (foo 1 2 42)
          "(foo/kw #:a 1 #:b 2)"
          #:c 42)
  ;;
 ;; Use a hash to create the struct
 (checker (thunk (hash->struct/kw foo/kw
                                  (hash 'a 1 'b 2 'c 3)))
          "(hash->struct/kw foo/kw (hash 'a 1 'b 2 'c 3)))")
  ;;
  ;; Use a hash that has more keys than you need:
 (checker (thunk (hash->struct/kw foo/kw  ; => (foo/kw #:a 1 #:b 2 #:c 3)
                                  (hash 'a 1 'b 2 'c 3 'd 5 'e 8)
                                  '(a b c)))
          "hash->struct/kw with restricted keys")
  ;;
  ;; Use a hash and rename some of the keys
 (checker (thunk (hash->struct/kw foo/kw  ; => (foo/kw #:a 1 #:b 2 #:c 3)
                                  (hash 'a 1 'b 2 'charlie 3)
                                  #:remap-keys (hash 'charlie 'c)))
          "hash->struct/kw with remapped key")
  ;;
  ;; Use a hash, only some of the keys, and rename some of the keys
 (checker (thunk (hash->struct/kw foo/kw (hash 'a 1 'b 2 'charlie 3 'd 5 'e 8)
                   '(a b charlie)
                   #:remap-keys (hash 'charlie 'c)))
          "hash->struct/kw with restricted and remapped keys")
  )
