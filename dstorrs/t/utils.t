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


(say "Done testing.")
