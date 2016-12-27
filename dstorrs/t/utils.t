#!/usr/bin/env racket

#lang at-exp rackjure

(require dstorrs/utils
		 dstorrs/test-more
		 )

(ok #t "testing harness works")

(for ((v (list 0 0.0 -0 -0.0 "" #f (void) null)))
  (ok (perl-false? v) @~a{@v is false}))

(test-suite
 "append-file"

 ;;    Create two test files
 (define source (make-temporary-file))
 (define dest   (make-temporary-file))

 (with-output-to-file
   source
   #:exists 'replace
   (thunk
    (display "67890")))

 (with-output-to-file
   dest
   #:exists 'append
   (thunk
    (display "12345")))

 (is (append-file source dest)
     10
     "got correct number of bytes after append")

 (is (with-input-from-file
       dest
       (thunk
        (port->string)))
     "1234567890"
     "got correct contents")

 (delete-file source)
 (delete-file dest)
 )

(test-suite
 "safe-hash-set"
 (define hash-imm (hash 'a 1 'b 2 'c 3))
 (ok (immutable? hash-imm) "using immutable hash for next test")
 (is (safe-hash-set hash-imm 'b 5)
     (hash 'a 1 'b 5 'c 3)
     "can handle an immutable hash")

 (define hash-mut (make-hash '((a . 1) (b . 2) (c . 3))))
 (define correct  (make-hash '((a . 1) (b . 5) (c . 3))))
 (ok (not (immutable? hash-mut)) "using mutable hash for next test")
 (ok (not (immutable? correct)) "using mutable hash for answer to next test")
 (is (safe-hash-set hash-mut 'b 5)
     correct
     "can handle a mutable hash")

 (dies (thunk (safe-hash-set #f 'a 7))
       "safe-hash-set requires a hash")
 )

(say "Done testing.")
