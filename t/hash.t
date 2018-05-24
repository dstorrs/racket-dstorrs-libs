#!/usr/bin/env racket

#lang racket

(require handy/hash
         handy/test-more)

(expect-n-tests 69)

(when #t
  (test-suite
   "safe-hash-remove"

   (define hash-imm (hash 'a 1 'b 2 'c 3))
   (define (hash-mut) (make-hash '((a . 1) (b . 2) (c . 3))))

   (is (safe-hash-remove hash-imm 'a)
       (hash 'b 2 'c 3)
       "(safe-hash-remove hash-imm 'a) worked")

   (is (safe-hash-remove hash-imm 'x)
       (hash 'a 1 'b 2 'c 3)
       "(safe-hash-remove hash-imm 'x) worked")

   (is (safe-hash-remove hash-imm 'a 'x)
       (hash 'b 2 'c 3)
       "(safe-hash-remove hash-imm 'a 'x) worked")

   (is (safe-hash-remove hash-imm '(a x))
       (hash 'b 2 'c 3)
       "(safe-hash-remove hash-imm '(a x)) worked (list of keys)")

   (is (safe-hash-remove (hash '(a x) 7 'b 3) '(a x) #:key-is-list #t)
       (hash 'b 3)
       "(safe-hash-remove (hash '(a x) 7 b 3) '(a x) #:key-is-list #t) worked")

   (is (safe-hash-remove (hash-mut) 'a)
       (make-hash '((b . 2) (c . 3)))
       "(safe-hash-remove hash-mut 'a) worked")

   (is (safe-hash-remove (hash-mut) 'x)
       (make-hash '((a . 1) (b . 2) (c . 3)))
       "(safe-hash-remove hash-mut 'x) worked")

   (define h (hash-mut))
   (let ((res (safe-hash-remove h 'a 'c 'x)))
     (ok (eq? h res) "safe-hash-remove returns the same hash when given a mutable hash")
     (is res
         (make-hash '((b . 2)))
         "(safe-hash-remove hash-mut 'a 'c 'x) worked"))


   (for ([func (list mutable-hash hash)]
         [type '(mutable immutable)])

     (is (safe-hash-remove (func))
         (func)
         (~a "(safe-hash-remove (" (object-name func) ")) is pathological but okay"))

     (is (safe-hash-remove (func) '())
         (func)
         (~a "(safe-hash-remove (" (object-name func) ") '()) is pathological but okay"))

     (define (make-it) (func 'a 7 'b 8))

     (is (safe-hash-remove (make-it) 'a)
         (func 'b 8)
         (~a "(safe-hash-remove h 'a) is okay when hash is " type))

     (is (safe-hash-remove (make-it) '(a))
         (func 'b 8)
         (~a "(safe-hash-remove h '(a)) is okay when hash is " type))

     (is (safe-hash-remove (make-it) 'a 'b)
         (func)
         (~a "(safe-hash-remove h 'a 'b) is okay when hash is " type))

     (is (safe-hash-remove (make-it) '(a b))
         (func)
         (~a "(safe-hash-remove h '(a b)) is okay when hash is " type))

     (is (safe-hash-remove (make-it) '(nosuchkey))
         (make-it)
         "removing a key that isn't there is a null op")

     (define (make-hash-with-list-key) (func '(a b) 7 'c 8))
     (is (safe-hash-remove (make-hash-with-list-key) 'c)
         (func '(a b) 7)
         "(safe-hash-remove (make-hash-with-list-key) 'c) okay")

     (is (safe-hash-remove (make-hash-with-list-key) '(a b))
         (make-hash-with-list-key)
         "Passing '(a b) correctly does not remove the key '(a b)")

     (is (safe-hash-remove (make-hash-with-list-key) '((a b)))
         (func 'c 8)
         "Passing '((a b)) DOES remove the key '(a b)")

     (is (safe-hash-remove (make-hash-with-list-key) '((a b) c))
         (func)
         "Passing '((a b) c) DOES remove both the key '(a b) and the key 'c"))

   (define weird-h   (hash '(foo bar) 'x 'a 7 'b 8))
   (is (safe-hash-remove weird-h '((foo bar) a))
       (hash 'b 8)
       "(safe-hash-remove weird-h '((foo bar) a)) => (hash 'b 8)")

   ));; test-suite

(test-suite
 "hash->mutable and hash->immutable"

 (define mut (make-hash '((a . 1))))
 (ok (not (immutable? mut)) "mut is mutable")
 (ok (immutable? (hash->immutable mut)) "hash->immutable mut is immutable")

 (define immut (hash 'a 1))
 (ok (immutable? immut) "immut is immutable")
 (ok (not (immutable? (hash->mutable immut))) "hash->mutable immut is mutable")
 )

(test-suite
 "hash-meld"

 (define x (hash 'a 1 'b 2))
 (define y (hash 'b 3 'c 4))
 (define z (hash 'c 5 'd 6))
 (define a (make-hash '((d . 7) (e . 8))))
 (is (hash-meld x y) (hash 'a 1 'b 3 'c 4) "hash-meld works with two immutable hashes")
 (is (hash-meld x y z a) (hash 'a 1 'b 3 'c 5 'd 7 'e 8) "hash-meld works with three immutable and + one mutable hashes (result is immutable since first hash was immutable)")
 (is (hash-meld a x y z)
     (hash->mutable (hash 'a 1 'b 3 'c 5 'd 6 'e 8))
     "hash-meld works with three immutable and + one mutable hashes (result is immutable since first hash was immutable)")
 )

(test-suite
 "safe-hash-set"
 (define hash-imm (hash 'a 1 'b 2 'c 3))
 (ok (immutable? hash-imm) "using immutable hash for next test")
 (is (safe-hash-set hash-imm 'b 5)
     (hash 'a 1 'b 5 'c 3)
     "can handle an immutable hash when specifying one key and one value")

 (is (safe-hash-set hash-imm 'a 3 'b 9 'd 8)
     (hash 'a 3 'b 9 'c 3 'd 8)
     "can handle an immutable hash when specifying multiple arguments")

 (throws (thunk (safe-hash-set hash-imm 'x))
         #px"safe-hash-set: contract violation"
         "safe-hash-set throws if given an odd number of arguments")




 (let ((hash-mut (make-hash '((a . 1) (b . 2) (c . 3)))))
   (is (safe-hash-set  hash-mut 'b 5)
       (make-hash '((a . 1) (b . 5) (c . 3)))
       "can handle a mutable hash"))

 (let* ((hash-mut  (make-hash '((a . 1) (b . 2) (c . 3))))
        (h (safe-hash-set hash-mut 'a 3 'b 9 'd 8)))
   (ok (eq? h hash-mut) "the hash returned from safe-hash-set is the same one sent in if you sent a mutable hash")
   (is h
       (hash->mutable (hash 'a 3 'b 9 'c 3 'd 8))
       "can handle a mutable hash when specifying multiple arguments"))

 (dies (thunk (safe-hash-set #f 'a 7))
       "safe-hash-set requires a hash for the first argument")
 )

(test-suite
 "mutable-hash"

 (let ((h (mutable-hash 'a 1)))
   (ok (and (hash? h) (not (immutable? h)))
       "(mutable-hash 'a 1)) works"))

 (let ((h (mutable-hash 'a 1 'b 2)))
   (ok (and (hash? h) (not (immutable? h)))
       "(mutable-hash 'a 1 'b 2)) works"))
 )

(when #t
  (test-suite
   "hash-keys->strings and hash-keys->symbols"
   (is (hash-keys->strings (hash "foo" 7 'bar 8))
       (hash "foo" 7 "bar" 8)
       "(hash-keys->strings works")

   (is (hash-keys->symbols (hash "foo" 7 'bar 8))
       (hash 'foo 7 'bar 8)
       "(hash-keys->symbols works")

   (is (hash-keys->strings (hash "foo" 7 'bar 8 0 'c))
       (hash "foo" 7 "bar" 8 "0" 'c)
       "(hash-keys->strings works on things with keys that are not symbol/string")

   (is (hash-keys->strings #:dash->underscore? #t (hash "foo-bar" 1 'bar-z 8))
       (hash "foo_bar" 1 "bar_z" 8)
       "hash-keys->strings #:dash->underscore? #t converts, e.g., 'foo-bar to \"foo_bar\"")

   (is (hash-keys->symbols (hash "foo" 7 'bar 8 0 'c))
       (hash 'foo 7 'bar 8 '|0| 'c)
       "(hash-keys->symbols works on things with keys that are not symbol/string")

   (is-type (hash-keys->symbols (mutable-hash "foo" 7 'bar 8))
            (negate immutable?)
            "hash-keys->symbols preserved the (im)mutability of the hash")

   (is-type (hash-keys->strings (hash "foo" 7 'bar 8))
            immutable?
            "hash-keys->strings preserved the (im)mutability of the hash")
   )
  )



(when #t
  (test-suite
   "hash->keyword-apply"
   (define (foo #:x x #:y y #:z z) (list x y z))
   (define (bar [a 1] #:x x #:y y #:z [z 0]) (list a x y z))

   (is (hash->keyword-apply foo (hash 'x 7 'y 8 'z 9))
       '(7 8 9)
       "hash->keyword-apply works with a correct hash and no positionals")

   (is (hash->keyword-apply bar (hash 'x 7 'y 8 'z 9) '(11))
       '(11 7 8 9)
       "hash->keyword-apply works with a correct hash and positionals")

   (is (hash->keyword-apply bar (hash 'x 7 'y 8))
       '(1 7 8 0)
       "hash->keyword-apply handles optional params")

   (throws (thunk (hash->keyword-apply bar (hash 'a 1 'x 7 'y 8 'z 9)))
           #px"procedure does not expect an argument with given keyword\\s+procedure: bar\\s+given keyword: #:a"
           "throws when the hash has a key that isn't a param")
   )
  )

(when #t
  (test-suite
   "hash-remap"

   (is (hash-remap (hash 'type 'fruit)   #:default (hash 'subtype 'apple))
       (apply hash '(type fruit subtype apple)))
   ))
