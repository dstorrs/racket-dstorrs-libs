#!/usr/bin/env racket

#lang at-exp racket

(require dstorrs/test-more
         dstorrs/utils
         dstorrs/fsmonitor
		 )

(ok #t "testing harness works")

(test-suite
 "fs-monitor-table-schema"

 (define pg (list
             @~a{CREATE TABLE "watched_files" (id BIGSERIAL,path TEXT NOT NULL UNIQUE ,modified TIMESTAMP NOT NULL,size BIGINT NOT NULL,"is_dir" BOOLEAN NOT NULL DEFAULT FALSE,CONSTRAINT "watched_files_pkey" PRIMARY KEY (id))}
             @~a{CREATE INDEX "watched_files_path_idx" ON "watched_files"(path)}
             ))
 (define sqlite (list
                 @~a{CREATE TABLE IF NOT EXISTS watched_files (id BIGINT PRIMARY KEY,path TEXT NOT NULL UNIQUE,modified TIMESTAMP NOT NULL,size BIGINT NOT NULL,is_dir BOOLEAN DEFAULT 'FALSE' NOT NULL)}
                 @~a{CREATE INDEX watched_files_path_idx ON watched_files(path)}
                 ))
 (define mysql (list
                @~a{CREATE TABLE IF NOT EXISTS watched_files(id BIGINT AUTO_INCREMENT,path TEXT NOT NULL UNIQUE ,modified TIMESTAMP NOT NULL,size BIGINT UNSIGNED NOT NULL,is_dir BOOLEAN NOT NULL DEFAULT FALSE,PRIMARY KEY (id))}
                @~a{CREATE INDEX watched_files_path_idx ON watched_files (path(50))}
                ))
 (for ((name '(pg postgres postgresql)))
   (is (fsmonitor-table-schema name)
       pg
       (~a "Got the proper schema for " name)))

 (is (fsmonitor-table-schema 'sqlite)
     sqlite
     "Got the proper schema for sqlite")

 (is (fsmonitor-table-schema 'mysql)
     mysql
     "Got the proper schema for mysql")

 ); test-suite

(test-suite
 "watch-dir, no recur"
 (ok #f "not done")
 )

(displayln "Done testing.")
