#!/usr/bin/env racket

#lang at-exp racket/base

(require handy/files/compression
         handy/test-more
         handy/utils
         racket/file
         racket/format
         racket/function)

(expect-n-tests 39)

(void (ok 1 "test harness working"))

(when #t
  (test-suite
   "gzip*"

   ; create and populate a temp file that we can (un)compress
   (with-temp-file #:path (make-test-file)
     (lambda (in-filepath)
       (define expected-contents (file->string in-filepath))
       #t
       ; create a temp file that we can write the compressed data to
       (with-temp-file
         (lambda (compressed-filepath)
           (is (file-size compressed-filepath) 0 "out file is empty")

           ;; gzip the file without specifying anything except the input
           ;; file.  The original should be left alone, the operation
           ;; should return the path you sent it to, that path should be
           ;; the standard path used by gzip, and it should be
           ;; non-existent before the operation and non-empty afterwards
           (define default-path (path-add-extension in-filepath ".gz" #"."))
           (delete-file-if-exists default-path)
           (is-false (file-exists? default-path) "to start, the default path doesn't exist")
           (ok (file-exists? in-filepath) "to start, the input file path exists")
           (gzip* in-filepath)
           (ok (file-exists? default-path) "compressed to expected path")
           (ok (file-exists? in-filepath) "source file was left untouched")
           (delete-file-if-exists default-path)
           ))
       ;;  gzip the file to a specified filepath without any other
       ;;  parameters.  Beforehand that file should not exist,
       ;;  afterwards it should be non-empty.  operation should
       ;;  return the specified path, original should be undisturbed
       (with-temp-file
         (lambda (compressed-filepath)
           (delete-file compressed-filepath)
           (is-false (file-exists? compressed-filepath) "before test, compressed-filepath isn't there")
           (is (gzip* in-filepath compressed-filepath)
               compressed-filepath
               "(gzip* in-filepath compressed-filepath) returns compressed-filepath")
           (ok (> (file-size compressed-filepath) 0)
               "successfully compressed to destination specified as a filepath")
           (delete-file-if-exists compressed-filepath)
           (ok (file-exists? in-filepath) "by default, original is not deleted")
           (is (file->string in-filepath)
               expected-contents
               "original was not disturbed")))


       ;; gzip the file to a specified path and delete the original.
       ;; Target is specified as a path-string. Then uncompress it back
       ;; to the original name without specifying the target
       (with-temp-file
         (lambda (compressed-filepath)
           (delete-file compressed-filepath)
           (is-false (file-exists? compressed-filepath) "before test, compressed-filepath isn't there")
           (is (gzip* in-filepath compressed-filepath #:remove-original? #t)
               compressed-filepath
               "(gzip* in-filepath compressed-filepath) returns compressed-filepath")
           (ok (> (file-size compressed-filepath) 0)
               "successfully compressed to destination specified as a filepath")
           (delete-file-if-exists compressed-filepath)
           (is-false (file-exists? in-filepath) "original was deleted as requested")))
       )); create input file
   #t
   )); test-suite, when


(when #t
  (test-suite
   "gunzip*"

   (with-temp-file #:path (make-test-file)
     (lambda (inpath)
       (define in-filepath (string->path inpath))
       (define expected-contents (file->string in-filepath))

       (define (setup)
         (define compressed-filepath (gzip* in-filepath #:remove-original? #t))
         (is-false (file-exists? in-filepath) "compressed and removed the original file")
         compressed-filepath)

       (define (uncompressed-correctly? label target-path zip-path should-remove?)
         (cond [(ok (file-exists? target-path) @~a{@|label|: after compression, original exists})
                (is (file->string target-path)
                    expected-contents
                    @~a{@|label|: file was uncompressed correctly})

                (cond [should-remove? (is-false (file-exists? zip-path)
                                                @~a{in block '@|label|': zip was correctly removed})]
                      [else  (ok (file-exists? zip-path)
                                 @~a{in block '@|label|': zip was correctly NOT removed})])]
               [else (say "...original file not present, skipping other tests in this block")])
         (delete-file-if-exists zip-path)
         (unless (equal? in-filepath target-path)
           (rename-file-or-directory target-path in-filepath)))


       ;;    Uncompress the file back to its original path without
       ;;    specifying anything other than the source.  The zip
       ;;    file should be removed by default
       (diag "compress files to the default filepath")
       (let ([compressed-filepath (setup)])
         (is (gunzip* compressed-filepath)
             in-filepath
             "(gunzip* compressed-filepath) returns in-filepath")
         (uncompressed-correctly? "no target specified" in-filepath compressed-filepath #t))

       ;;    Uncompress the file to a specified path with target
       ;;    specified as path-string.  The zip file should be removed
       ;;    by default
       (diag "compress files to a target specified by path-string")
       (let ([compressed-filepath (setup)]
             [target-filepath (make-temporary-file)])
         (is (gunzip* compressed-filepath target-filepath)
             target-filepath
             "(gunzip* compressed-filepath target-filepath) returns correct target")
         (uncompressed-correctly? "target specified as path"  target-filepath compressed-filepath #t))



       ;;    Uncompress the file to a specified path with target
       ;;    specified by generator.  The zip file should be removed
       ;;    by default
       (diag "compress files to a target specified by generator")
       (let ([compressed-filepath (setup)]
             [target-filepath (path-string->string (make-temporary-file))])
         (is (gunzip* compressed-filepath (lambda (x y) target-filepath))
             target-filepath
             "(gunzip* compressed-filepath <generator>) returns correct target")
         (uncompressed-correctly? "target specified as path"  target-filepath compressed-filepath #t))



       ;;    Uncompress the file to a specified path with target
       ;;    specified by filepath. Do NOT remove the zip
       (let ([compressed-filepath (setup)]
             [target-filepath (make-temporary-file)])
         (is (gunzip* compressed-filepath target-filepath #:remove-zip? #f)
             target-filepath
             "(gunzip* compressed-filepath target) returns correct target")
         (uncompressed-correctly? "target specified as path"  target-filepath compressed-filepath #f)
         (delete-file-if-exists compressed-filepath)
         )


       ;;  Verify that if you try to unzip something that is not a zip
       ;;  file then you will get an exn:fail:gunzip back instead of
       ;;  an exn:fail.  Any other error will come back undisturbed
       (with-temp-file
         (lambda (the-path)
           (throws (thunk (gunzip* the-path))
                   exn:fail:gunzip?
                   "trying to unzip something that is not a zip file will thrown an exn:fail:gunzip, not an exn:fail")
           (delete-file-if-exists the-path)
           (throws (thunk (gunzip* the-path))
                   exn:fail:filesystem?
                   "trying to unzip something that doesn't exist is an exn:fail:filesystem?, not an exn:fail:gunzip")))

           ))))
