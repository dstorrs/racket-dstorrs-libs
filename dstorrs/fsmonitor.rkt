#lang at-exp rackjure

(require db
         dstorrs/try
         dstorrs/utils
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

(define/contract (watch-dir-or-tree dir handler [handler-arg #f] #:recursive [recursive #t])
  (->* (path-string? (-> path-string? any/c any))
       (any/c #:recursive boolean?)
       any)

  ;;    Locate all the directories to watch. If recursive is #f then
  ;;    that's just dir.  If recursive is #t then it's dir and all
  ;;    directories below dir.
  (define dirs-to-watch
    (if recursive
        (fold-files  (lambda (item type acc) (if (equal? type 'dir) (cons item acc) acc))
                     '()
                     dir)
        (list dir))
    )


  (define (evt-maker dir)
    (handle-evt (filesystem-change-evt dir)
                (lambda (e)
                  (handler dir handler-arg))))

  (define (work)
    (let loop ()
      (apply sync (map evt-maker dirs-to-watch))
      (loop)))

  (void (thread work))
  )

(provide (all-defined-out))
