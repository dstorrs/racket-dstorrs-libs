#lang at-exp rackjure

(require db
         dstorrs/try
         kw-utils/partial
         )

;;----------------------------------------------------------------------

(define/contract (fsmonitor-setup-sql db-type)
  (-> (or/c 'pg 'postgres 'postgresql 'sqlite)
      (non-empty-listof string?))
  (cond [(ormap (curry equal? db-type) '(pg postgres postgresql))
         (list
          @~a{
              CREATE TABLE "watched_files"
                     (
                      id BIGSERIAL,
                         dir TEXT NOT NULL,
                         filename TEXT,
                         modified BIGINT NOT NULL,
                         "file_size_bytes" BIGINT NOT NULL,
                         "is_missing" BOOL NOT NULL DEFAULT false,
                         "last_checked" BIGINT NOT NULL,
                         CONSTRAINT "watched_files_pkey" PRIMARY KEY (id)
                         )}
          @~a{ALTER TABLE "watched_files" ADD CONSTRAINT "watched_files_cns_1" UNIQUE (dir,filename)}
          @~a{CREATE INDEX "watched_files_dir_idx" ON "watched_files"(dir)}
          )
         ]
        [(equal? db-type 'sqlite)
         (list

          @~a{CREATE TABLE IF NOT EXISTS watched_files
                     (
                      id BIGINT PRIMARY KEY,
                         dir TEXT NOT NULL,
                         filename TEXT NOT NULL,
                         is_dir BOOL DEFAULT 'false' NOT NULL,
                         modified BIGINT NOT NULL,
                         file_size_bytes BIGINT NOT NULL,
                         is_missing BOOL DEFAULT 'false' NOT NULL,
                         last_checked BIGINT NOT NULL
                         )}
         @~a{CREATE UNIQUE INDEX IF NOT EXISTS watched_files_cns_1 ON watched_files (dir,filename)}
         @~a{CREATE INDEX watched_files_dir_idx ON watched_files(dir)}
         )]))

;;----------------------------------------------------------------------

(define/contract (ensure-fsmonitor-table-exists db db-type)
  (-> connection? (or/c 'pg 'postgres 'postgresql 'sqlite) any)

  ;;    Test if the table exists before we do anything.  If not, set it up.
  (try  [(query-maybe-value db "select 1 from watched_files limit 1")]
        [catch (exn:fail?
                (lambda (e)
                  (call-with-transaction
                   db
                   (thunk
                    (for ((stmt (fsmonitor-setup-sql db-type)))
                      (query-exec db stmt))))
                  ;;
                  ;;    Verify that the table was set up, throw if not.
                  (query-maybe-value db "select 1 from watched_files limit 1")))])
  )

;;----------------------------------------------------------------------

(define/contract (watch-dir dir dbh-maker handler)
  (-> path-string?
      (-> connection?)                  ;; dbh-maker is a thunk that returns a dbh
      (-> path-string? connection? any) ;; proc takes dir and a dbh
      any)

  ;;    Create a watch-filesystem event for this directory
  ;;    Put it in a thread, make the thread loop forever
  (thread
   (thunk
    (do ()
        (#f)
      (let ((change (sync (filesystem-change-evt dir))))
        (handler dir (dbh-maker)))
      ))))

;;----------------------------------------------------------------------

(define/contract (watch-dir-tree dir dbh-maker handler #:recursive [recursive #t])
  (->* (path-string? (-> connection?) (-> any))
       (#:recursive boolean?)
       any)

  ;;    Watch the specified directory.  If recursive was set, walk
  ;;    down into the directory looking for other directories, then
  ;;    recur on them.
  (watch-dir dir dbh-maker handler)
  (when recursive
    (fold-files (lambda (fpath type acc)
                  (when (equal? 'dir type)
                    (watch-dir-tree fpath dbh-maker handler)))
                '()
                dir))
  )

(provide (all-defined-out))
