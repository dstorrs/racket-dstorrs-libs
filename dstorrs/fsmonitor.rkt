#lang at-exp racket

(require db)

;;----------------------------------------------------------------------

(define/contract (fsmonitor-table-schema db-type)
  (-> (or/c 'pg 'postgres 'postgresql 'sqlite 'mysql)
      (non-empty-listof string?))
  (cond [(ormap (curry equal? db-type) '(pg postgres postgresql))
         (list
           @~a{CREATE TABLE "watched_files" (id BIGSERIAL,path TEXT NOT NULL UNIQUE ,modified TIMESTAMP NOT NULL,size BIGINT NOT NULL,"is_dir" BOOLEAN NOT NULL DEFAULT FALSE,CONSTRAINT "watched_files_pkey" PRIMARY KEY (id))}
           @~a{CREATE INDEX "watched_files_path_idx" ON "watched_files"(path)})
         ]
        [(equal? db-type 'sqlite)
         (list
          @~a{CREATE TABLE IF NOT EXISTS watched_files (id BIGINT PRIMARY KEY,path TEXT NOT NULL UNIQUE,modified TIMESTAMP NOT NULL,size BIGINT NOT NULL,is_dir BOOLEAN DEFAULT 'FALSE' NOT NULL)}
          @~a{CREATE INDEX watched_files_path_idx ON watched_files(path)}
          )]
        [(equal? db-type 'mysql)
         (list
          @~a{CREATE TABLE IF NOT EXISTS watched_files(id BIGINT AUTO_INCREMENT,path TEXT NOT NULL UNIQUE ,modified TIMESTAMP NOT NULL,size BIGINT UNSIGNED NOT NULL,is_dir BOOLEAN NOT NULL DEFAULT FALSE,PRIMARY KEY (id))}
          @~a{CREATE INDEX watched_files_path_idx ON watched_files (path(50))}
          )]))

;;----------------------------------------------------------------------

(define/contract (watch-dir dir dbh-maker [handler identity] #:recursive [recur #t])
  (->* (path-string? (-> connection?))
       ((-> filesystem-change-evt? any)
        #:recursive boolean?)
       any)
  (raise "not implemented")
  )


(provide (all-defined-out))
