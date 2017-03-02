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
 "->string"

 (is (->string 'foo) "foo" "worked for symbol foo")
 (is (->string "foo") "foo" "worked for string foo")
 (is (->string 8) "8" "worked for int 8")
 (is (->string 8.0) "8.0" "worked for num 8.0")
 (is (->string +inf.0) "+inf.0" "worked for num +inf.0")
 (is (->string -inf.0) "-inf.0" "worked for num -inf.0")
 (is (->string +nan.0) "+nan.0" "worked for num +nan.0")
 (is (->string -nan.0) "+nan.0" "worked for num -nan.0") ; weird conversion but correct
 (is (->string (list #\A)) "A" "worked for (list char)")
 (is (->string (list "x" "y" "z")) "xyz" "worked for (list string)")
 (is (->string (vector "x" "y" "z")) "xyz" "worked for (vector string, string, string)")
 (is (->string (vector 8 "y" "z")) "8yz" "worked for (vector num, string, string)")

 (throws (thunk (->string #f))
         exn:fail:contract?
         "(->to-string #f) throws")
 (struct test-struct (x))
 (throws (thunk (->string (test-struct 'x)))
         exn:fail:contract?
         "(->to-string struct) throws")
 (throws (thunk (->string (exn:fail "foo" (current-continuation-marks))))
         exn:fail:contract?
         "(->to-string exn:fail) throws")
 (throws (thunk (->string (list 8 8 #f)))
         exn:fail:contract?
         "(->to-string (list 8 8 #f)) throws")
 (throws (thunk (->string (vector 8 8 #f)))
         exn:fail:contract?
         "(->to-string (list 8 8 #f)) throws")
 )

(test-suite
 "rand-val"
 (for ((v (list 1000 "foo" 'bar #\A (list 1 2 3) (vector 1 2 3))))
   (let ((r (rand-val v)))
     (is-type r string? (~a "got expected type for " r))
     (ok (regexp-match (pregexp (~a (->string v) "-\\d+")) r)
         (~a "rand-value is as expected for " r))
     )
   )
 (is-type (rand-val) string? "generic (rand-val) yields string")
 (ok (regexp-match (pregexp "\\d+") (rand-val))
     (~a "rand-value is as expected for (rand-val)"))
 (is-type (rand-val #:post string->number) number? "got expected type for string->number")
 (let ((lst (rand-val #:post string->list)))
   (is-type lst list? "got list from #:post string->list")
   (ok (andmap char? lst)
       "when post processing with string->list, we get a list of characters, as expected"))
 
 );;test-suite

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
 
 (is (safe-hash-remove (hash-mut) 'a)
     (make-hash '((b . 2) (c . 3)))
     "(safe-hash-remove hash-mut 'a) worked")

 (is (safe-hash-remove (hash-mut) 'x)
     (make-hash '((a . 1) (b . 2) (c . 3)))
     "(safe-hash-remove hash-mut 'x) worked")
 );; test-suite

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

(test-suite
 "symbols->keywords"
 (is (symbols->keywords '(foo bar baz))
     '(#:bar #:baz #:foo)
     "correctly converted '(foo bar baz)")
 )

(say "Done testing.")
