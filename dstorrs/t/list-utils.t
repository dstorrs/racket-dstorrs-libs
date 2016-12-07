#!/usr/bin/env racket

#lang at-exp rackjure

(require "../list-utils.rkt"
		 "../test-more.rkt")

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
 (is (get l '(188) -11) -11
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

 (is (firstn '(foo bar)) 'foo "firstn '(foo bar) is 'foo")
 (is (firstn '()) '() "firstn '() is '()")

 (is (restn '(foo bar)) '(bar) "firstn '(foo bar) is '(bar)")
 (is (restn '()) '() "restn '() is '()")
 )


(test-suite
 "find-contiguous-runs"
 (define nums '(1 2 3 5 7 200 201 202 203))

 (is (find-contiguous-runs nums)
     '((1 2 3) (5) (7) (200 201 202 203))
     "correctly found runs in a list of numbers")

 (is
  (find-contiguous-runs (map (curry vector->dict '(chunk-hash chunk-num chunkdir-path scratchdir-path))
                             '(#("hash-7347" 6 "/foo/bar-2541" #f)
                               #("hash-1983" 8 "/foo/bar-2542" #f)
                               #("hash-5917" 9 "/foo/bar-9014" #f)))
                        #:key (lambda (h) (hash-ref h 'chunk-num)))
  '((#hash((chunk-hash . "hash-7347") (chunk-num . 6) (chunkdir-path . "/foo/bar-2541") (scratchdir-path . #f)))
    (#hash((chunk-hash . "hash-1983") (chunk-num . 8) (chunkdir-path . "/foo/bar-2542") (scratchdir-path . #f))
     #hash((chunk-hash . "hash-5917") (chunk-num . 9) (chunkdir-path . "/foo/bar-9014") (scratchdir-path . #f))))
  "correctly found runs in a list of hashes")
 )

(test-suite
 "list->dict and vector->dict"

 (define data '((a . 1) (b . 2) (c . 3)))

 (displayln (list->dict '(a b c)
                        '(1 2 3)))
 (displayln (make-hash data))
 (is (list->dict '(a b c)
                 '(1 2 3))
     (make-hash data)
     "(list->dict  '(a b c) '(1 2 3)) works (default dict-maker)")

 (is (list->dict '(a b c)
                 '(1 2 3)
                 #:dict-maker make-hasheq)
     (make-hasheq '((a . 1) (b . 2) (c . 3)))
     "(list->dict  '(a b c) '(1 2 3)) works with make-hasheq")

 (displayln (list->dict '(a b c)
                 '(1 2 3)
                 #:transform (lambda (d)
                               (apply hash (for/fold ((acc '()))
                                               ((k (hash-keys  d)))
                                       (append (list (string->symbol (string-append "key-" (symbol->string k)))
                                                     (add1 (hash-ref d k)))
                                               acc))))))

 (is (list->dict '(a b c)
                 '(1 2 3)
                 #:transform (lambda (d)
                               (displayln (~a "IN TRANSFORM, dict: " d))
                               (define res (apply hash (for/fold ((acc '()))
                                               ((k (hash-keys  d)))
                                       (append (list (string->symbol (string-append "key-" (symbol->string k)))
                                                     (add1 (hash-ref d k)))
                                               acc))))
                                 (displayln (~a "IN TRANSFORM, res : " res))
                                 res))
     (apply hash '(key-a 2 key-b 3 key-c  4))
     "list->dict can transform keys and values")

 (is (vector->dict '(a b c)
                   (vector 1 2 3))
     (make-hash '((a . 1) (b . 2) (c . 3)))
     "(vector->dict  '(a b c) (vector 1 2 3)) works")

 (is (vector->dict '(a b c)
                   (vector 1 2 3)
                   #:transform (lambda (d)
                                 (for ((k (hash-keys d))) (hash-set! d k (add1 (d k)))
                                      )
                                 d))
     (make-hash '((a . 2) (b . 3) (c . 4)))
     "vector->dict accepts transformer")
 )
(exit)
