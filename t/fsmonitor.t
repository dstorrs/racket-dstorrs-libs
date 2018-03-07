#!/usr/bin/env racket

#lang at-exp racket

(require "../test-more.rkt"
         "../utils.rkt"
         "../fsmonitor.rkt"
         db
         racket/runtime-path
		 )



(define-runtime-path thisdir ".")
(define-runtime-path testdir "./data-fsmonitor-test-data")

(define db-path (build-path thisdir "test_db.sqlite"))

(define (new-db)
  (when (file-exists? db-path) (delete-file db-path))
  (sqlite3-connect db-path #:mode 'create))


(define (refresh-test-data)
  ;;    Delete all the files that are currently there
  (delete-directory/files testdir)

  ;;    Recreate the test files:
  ;; <testdir>/{0-2}/{a-c}
  (make-directory* testdir)

  (define data "foo")
  (define res '()) ;; There must be a smoother way to do this
  (for ((i 2))
    (define dir (build-path testdir (number->string i)))
    (make-directory* dir)
    (for ((name '("a" "b" "c")))
      (define fpath (build-path dir name))
      (display-to-file data fpath)
      (cons (vector (path->string fpath) (length data)))))

  (reverse res)
  )

;;----------------------------------------------------------------------

(ok #t "testing harness works")

(test-suite
 "compare-directory"

 (ok #f "not implemented")
 )

;;----------------------------------------------------------------------

(test-suite
 "watch-dir, not recursive"

 (ok #f "not implemented")
 )

;;----------------------------------------------------------------------

(test-suite
 "watch-dir recursive"

 (ok #f "not implemented")
 )



(done-testing)
