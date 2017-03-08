#!/usr/bin/env racket

#lang racket

(require "../test-more.rkt"
		 "../sql.rkt"
		 )

(void (ok #t "test harness is working"))

(test-suite
 "join-related functions"

 (define c "collaborations")
 (define f "files")
 
 (is (join-table-name c f)
     "collaborations_to_files"
     "join-table-name works")

 (is (join-tables c f)
     "collaborations c JOIN collaborations_to_files c2f ON c.id = c2f.collaboration_id JOIN files f ON c2f.file_id = f.id"
     "join-clause works")
 )


