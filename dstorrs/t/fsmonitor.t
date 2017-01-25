#!/usr/bin/env racket

#lang at-exp racket

(require dstorrs/test-more
         dstorrs/utils
         dstorrs/fsmonitor
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
 "fs-monitor-table-schema"

 (define pg (list
             @~a{CREATE TABLE "watched_files" (id BIGSERIAL,path TEXT NOT NULL UNIQUE ,last_modified TIMESTAMP NOT NULL,file_size_bytes BIGINT NOT NULL,"is_dir" BOOLEAN NOT NULL DEFAULT FALSE,CONSTRAINT "watched_files_pkey" PRIMARY KEY (id))}
             @~a{CREATE INDEX "watched_files_path_idx" ON "watched_files"(path)}
             ))
 (define sqlite (list
                 @~a{CREATE TABLE IF NOT EXISTS watched_files (id BIGINT PRIMARY KEY,path TEXT NOT NULL UNIQUE,last_modified TIMESTAMP NOT NULL,file_size_bytes BIGINT NOT NULL,is_dir BOOLEAN DEFAULT 'FALSE' NOT NULL)}
                 @~a{CREATE INDEX watched_files_path_idx ON watched_files(path)}
                 ))
 (for ((name '(pg postgres postgresql)))
   (is (fsmonitor-setup-sql name)
       pg
       (~a "Got the proper schema for " name)))

 (is (fsmonitor-setup-sql 'sqlite)
     sqlite
     "Got the proper schema for sqlite")
 ); test-suite

;;----------------------------------------------------------------------

(test-suite
 "take-directory-snapshot"

 (define db (new-db))
 (refresh-test-data)

 (let ((numrows (query-rows db "select count(1) from watched_files")))
   (is numrows 0  "started off with no rows"))

 (define snapshot (take-directory-snapshot testdir))

 (define base (path->string testdir))

 (let ((rows (query-rows db "select path, file_size_bytes from watched_files order by path")))
   (is rows
       snapshot
       "take-directory-snapshot did the right thing"))
 )

;;----------------------------------------------------------------------

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



(displayln "Done testing.")
