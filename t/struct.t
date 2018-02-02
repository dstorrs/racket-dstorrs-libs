#!/usr/bin/env racket

#lang at-exp racket

(require "../struct.rkt"
         "../test-more.rkt"
         )

(void (ok 1 "test harness is working"))

;; Define a struct type
(struct/kw foo (a b [c 42]) #:transparent)
(define (checker thnk description #:a [a-val 1] #:b [b-val 2] #:c [c-val 3])
  (displayln (~a "entering checker"))
  (match (thnk)
    [(struct* foo ([a a] [b b] [c c]))
     (begin
       (is a a-val (~a description " => got correct a"))
       (is b b-val (~a description " => got correct b"))
       (is c c-val (~a description " => got correct c")))]
    [_ (ok #f (~a description " => did not match"))])
  (displayln (~a "leaving checker"))
  )

(when #t
  (test-suite
   "struct/kw"

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

   ;; (define (make-foo . lst)
   ;;   (hash->struct/kw foo/kw
   ;;                    (make-hash (map cons (list 'a 'b 'c) lst))))

   ;; (define test-struct  (make-foo 1 5 9))
   ;; (is test-struct (foo 1 5 9) "make-foo worked")

   ;; (for ([setter (list set-foo-a set-foo-b set-foo-c)]
   ;;       [getter (list foo-a foo-b foo-c)]
   ;;       [new-val 88])

   ;;   (is (getter (setter test-struct new-val))
   ;;       new-val
   ;;       (~a "setter worked: " setter)))
   ))


(when #t
  (test-suite
   "hash->struct/kw, w/ and w/o restricted keys"
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
   ) ; test-suite
  ) ; when

(when #t
  (test-suite
   "hash->struct/kw with key remapping"

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
   ;;
   ;; Use a hash and rename the keys in non-alphabetic ways
   (define arg (hash 'alpha 1 'bravo 2 'charlie 3 'delta 5 'echo 8))
   (struct/kw bar (a b c d e) #:transparent)
   (displayln arg)
   (define res (hash->struct/kw bar/kw arg
                                #:remap-keys (hash 'echo    'a
                                                   'delta   'b
                                                   'charlie 'd
                                                   'bravo   'c
                                                   'alpha   'e
                                                   )))
   (is (bar-a res) (hash-ref arg 'echo)    "echo was remapped to a")
   (is (bar-b res) (hash-ref arg 'delta)   "delta was remapped to b")
   (is (bar-c res) (hash-ref arg 'bravo)   "bravo was remapped to c")
   (is (bar-d res) (hash-ref arg 'charlie) "charlie was remapped to d")
   (is (bar-e res) (hash-ref arg 'alpha)   "alpha was remapped to e")
   );test-suite
  );when


;; (when #t
;;   (test-suite
;;    "verify-data"

;;    (struct foo (a b c))
;;    (define x (foo 1 2 3))
;;    (define y (foo 0 2 3))

;;    (is-type x foo? "x is a foo")
;;    (is-type y foo? "y is a foo")

;;    (isnt x y "x and y are not equal?")
;;    (ok (verify-data #:data x
;;                     #:tests (list (cons foo-b 2) (cons foo-c 3)))
;;        "validates when given list of tests")
;;    (ok (verify-data #:data x #:type foo?) "validates when given a type predicate")
;;    ))



(done-testing) ; this should be the last line in the file
