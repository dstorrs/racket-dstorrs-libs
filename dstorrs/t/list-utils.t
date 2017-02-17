#!/usr/bin/env racket

#lang at-exp rackjure

(require "../list-utils.rkt"
         "../test-more.rkt"
         )

(ok 1 "test harness is working")

(test-suite
 "original tests"
 (define l '(foo "bar" ("baz" (quux))))
 (define h (make-hash
            `(("foo" . "bar")
              (baz . 7)
              (quux . (foo bar))
              (blag . ,(make-hash '(["baz" . "jaz"]))))))

 (is (car l) 'foo "car l is 'foo")
 (is (get l 0) 'foo "(get l 0) is 'foo")
 (is (get l '(0)) 'foo "(get l '(0)) is 'foo")
 (is (get l '(1)) "bar" "(get l '(1)) is \"bar\"")
 (is (get l '(2)) '("baz" (quux)) "(get l '(2)) is '(\"baz\" (quux))")
 (is (get l '(2 0)) "baz" "(get l '(2 0)) is '\"baz\"")
 (throws (lambda () (get l '(188 0)))
         #px"list-ref: index too large for list"
         "(get l '(188 0)) throws: index too large for list")

 (is (get l '(188) -11)
     -11
     "(get l '(188 0) -11) returns -11; the index was too big so it defaulted")

 (for ((k '("foo" baz quux blag (blag "baz")))
       (v `("bar" 7 (foo bar) ,(make-hash '(["baz" . "jaz"])))))
   (is (get h `(,k)) v (format "(get h ~a) is ~a" k v)))

 (is (get h 'quux) '(foo bar) "(get h '(quux 0) is '(foo bar)")
 (is (get h '(quux 0)) 'foo "(get h '(quux 0) is foo")

 (throws (lambda () (get h '(jaz)))
         #px"no value found for key"
         "throws on non-existent key 'jaz")
 (throws (lambda () (get h '(blag jaz)))
         #px"no value found for key"
         "throws on non-existent key '(blag jaz)")

 (is (get h '(jaz) "not found")
     "not found"
     "defaults correctly if key was not found in hash")

 (is (autobox "foo") '("foo") "(autobox \"foo\" returns '(\"foo\")")
 (is (autobox '("foo")) '("foo") "(autobox '(\"foo\") returns '(\"foo\")")

 (is (atom? "foo") #t "atom? detects string as atom")
 (is (atom? 7) #t "atom? detects string as atom")
 (is (atom? (hash 'a 7)) #t "atom? detects hash as atom")
 (is (atom? '("foo")) #f "atom? detects list as non-atom")

 (is (remove-nulls '(foo bar)) '(foo bar) "remove-nulls leaves list unchanged if it contains no null list")
 (is (remove-nulls '(foo () bar)) '(foo bar) "remove-nulls removes one null")
 (is (remove-nulls '(foo (()) bar)) '(foo (()) bar) "remove-nulls does not remove (())")

 (for ((v `(#f #t "foo" 7 list? () ,(make-hash '((x . 7))) ,(make-vector 8))))
   (not-ok (list/not-null? v) (format "list/not-null? ~a is #f" v)))

 (for ((v '((foo) (()))))
   (ok (list/not-null? v) (format "(list/not-null? ~a is #t" v)))

 (is (get '(a b c) '(0)) 'a)

 (is (safe-first '(foo bar)) 'foo "safe-first '(foo bar) is 'foo")
 (is (safe-first '()) '() "safe-first '() is '()")

 (is (safe-rest '(foo bar)) '(bar) "safe-first '(foo bar) is '(bar)")
 (is (safe-rest '()) '() "safe-rest '() is '()")
 )


(test-suite
 "find-contiguous-runs"
 (define nums '(1 2 3 5 7 200 201 202 203))

 (is (find-contiguous-runs nums)
     '((1 2 3) (5) (7) (200 201 202 203))
     "correctly found runs in a list of numbers")

 (define conv (curry vector->dict '(chunk-hash chunk-num chunkdir-path scratchdir-path)))
 (define vec-list '(#("hash-7347" 6 "/foo/bar-2541" #f)
                    #("hash-1983" 8 "/foo/bar-2542" #f)
                    #("hash-5917" 9 "/foo/bar-9014" #f)))

 (is (find-contiguous-runs (map conv vec-list)
                           #:key (lambda (h) (hash-ref h 'chunk-num)))
     (list (list (conv (first vec-list)))
           (list (conv (second vec-list))
                 (conv (third vec-list))))
     "correctly found runs in a list of hashes")
 )

(test-suite
 "list->dict and vector->dict"

 (define data '((a . 1) (b . 2) (c . 3)))

 (is (list->dict '(a b c)
                 '(1 2 3))
     (make-hash data)
     "(list->dict  '(a b c) '(1 2 3)) works (default dict-maker)")

 (is (list->dict '(a b c)
                 '(1 2 3)
                 #:dict-maker make-hasheq)
     (make-hasheq '((a . 1) (b . 2) (c . 3)))
     "(list->dict  '(a b c) '(1 2 3)) works with make-hasheq")

 (is (list->dict '(a b c)
                 '(1 2 3)
                 #:transform-data (lambda (k v) (cons k (add1 v))))
     (make-hash '((a . 2) (b . 3) (c . 4)))
     "list->dict can transform the data before creation")

 (is (list->dict '(a b c)
                 '(1 2 3)
                 #:transform-dict
                 (lambda (d)
                   (apply hash
                          (for/fold ((acc '()))
                                    ((k (hash-keys  d)))
                            (append (list (string->symbol (string-append "key-" (symbol->string k)))
                                          (add1 (hash-ref d k)))
                                    acc)))))
     (apply hash '(key-a 2 key-b 3 key-c  4))
     "list->dict can transform the dict after creation")

 (is (list->dict null
                 '(65 66 67)
                 #:make-keys integer->char)
     (make-hash '((#\A . 65) (#\B . 66) (#\C . 67)))
     "(list->dict null '(65 66 67) #:make-keys integer->char) works")

 (is (list->dict '("foo" "bar" "baz")
                 '(65 66 67)
                 #:make-keys integer->char)
     (make-hash '((#\A . 65) (#\B . 66) (#\C . 67)))
     "(list->dict '(foo bar baz) '(65 66 67)  #:make-keys integer->char) works")
  
 (is (vector->dict '(a b c)
                   (vector 1 2 3))
     (make-hash '((a . 1) (b . 2) (c . 3)))
     "(vector->dict  '(a b c) (vector 1 2 3)) works")

 (is (vector->dict '(a b c)
                   (vector 1 2 3)
                   #:transform-dict (lambda (d)
                                      (for ((k (hash-keys d)))
                                        (hash-set! d k (add1 (d k)))
                                        )
                                      d))
     (make-hash '((a . 2) (b . 3) (c . 4)))
     "vector->dict accepts transformer")
 )

(test-suite
 "flatten/convert"
 (is (flatten/convert vector->list (list (vector 1)(vector 2)(vector 3)))
     '(1 2 3)
     "converted vectors of one int to list of ints")

 (is (flatten/convert add1 (list 1 2 3))
     '(2 3 4)
     "incremented list")

 (is (flatten/convert (compose length hash-keys)
                      (list (hash 'a 1 'b 2)
                            (hash 'c 2 'd 3 'e 4)))
     '(2 3)
     "counted  list")

 )

(test-suite
 "unique"
 (is (unique '()) '() "null ")
 (is (unique '(1)) '(1) "'(1) ")
 (is (unique '(2 1)) '(2 1) "(2 1) ")
 (is (unique '(2 2 1)) '(2 1) "(2 2 1) ")
 (is (unique '(2 foo 2 1)) '(2 foo 1) "(2 foo 2 1) ")
 (is (unique (list 2 '() 2 1)) '(2 1) "(2 () 2 1) ")
 (is (unique (list 2 (hash) 2 1)) (list 2 (hash) 1) "(2 (hash) 2 1) ")
 (is (unique (list 2 (list 0 (vector)) 2 1)) (list 2 (list 0 (vector)) 1) "(2 (0 (vector)) 2 1)")
 (is (unique '(2 #t 2 1 #t)) '(2 #t 1) "(2 #t 2 1) ")
 (is (unique '(2 #f 2 1 #t)) '(2 #f 1 #t) "(2 #f 2 1 #t) ")
 (is (unique '(2 "apple" 2 "apple" 1)) '(2 "apple" 1) "(2 apple 2 apple 1) [apple => string]")
 )

(test-suite
 "disjunction"

 (define (test-disj d1 d2 correct [msg ""])
   (define disj (disjunction d1 d2))

   (is-type disj dict-disjunction? "got correct struct type")

   ;; (struct dict-disjunction (different
   ;;                           only-in-first
   ;;                           only-in-second
   ;;                           dict-first
   ;;                           dict-second) #:transparent)
   (is disj
       correct
       (format "got correct disjunction for ~a: ~a and ~a" msg d1 d2))

   (for ((f (list dict-disjunction-different
                  dict-disjunction-only-in-first
                  dict-disjunction-only-in-second
                  dict-disjunction-dict-first
                  dict-disjunction-dict-second)))
     (is (f disj)
         (f correct)
         (~a "accessor " (object-name f) " works")))
   )

 (let ((d1 (apply hash '(a 1 b 2 d 4)))
       (d2 (apply hash '(a 1 b 3 e 5))))
   (test-disj d1
              d2
              (dict-disjunction  (make-hash '((b . (2 3)))) ;; different
                                 (make-hash '((d . 4)))     ;; first
                                 (make-hash '((e . 5)))     ;; second
                                 d1
                                 d2)))

 (let ((d1 (hash))
       (d2 (hash)))
   (test-disj d1
              d2
              (dict-disjunction (make-hash)
                                (make-hash)
                                (make-hash)
                                d1
                                d2)
              "hash, hash"
              ))

 (let ((d1 (make-hash))
       (d2 (make-hash)))
   (test-disj d1
              d2
              (dict-disjunction (make-hash)
                                (make-hash)
                                (make-hash)
                                d1
                                d2)
              "make-hash, make-hash"))
 
 (let ((d1 (make-hash))
       (d2 (hash)))
   (test-disj d1
              d2
              (dict-disjunction (make-hash)
                                (make-hash)
                                (make-hash)
                                d1
                                d2)
              "make-hash, hash"))

 (let ((d1 (apply hash '(a 2 b 3 e 5)))
       (d2 (apply hash '(a 1 b 3 e 5))))
   (test-disj d1
              d2
              (dict-disjunction  (make-hash '((a . (2 1)))) ;; different
                                 (make-hash)
                                 (make-hash)
                                 d1
                                 d2)))

 (let ((d1 (apply hash '(a 2)))
       (d2 (apply hash '(a 1 b 3 e 5))))
   (test-disj d1
              d2
              (dict-disjunction  (make-hash '((a . (2 1)))) ;; different
                                 (make-hash)
                                 (make-hash '((b . 3) (e . 5)))
                                 d1
                                 d2)))
 (let* ((h (hash 'a 1))
        (d1 (apply hash '(a 2)))
        (d2 (apply hash (list 'a h))))
   (test-disj d1
              d2
              (dict-disjunction  (make-hash `((a . (2 ,h))))
                                 (make-hash)
                                 (make-hash)
                                 d1
                                 d2)))
               
 );; test-suite
