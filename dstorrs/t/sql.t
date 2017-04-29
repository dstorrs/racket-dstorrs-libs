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

 (is (join-clause c f)
     "collaborations c JOIN collaborations_to_files c2f ON c.id = c2f.collaboration_id JOIN files f ON c2f.file_id = f.id"
     "join-clause works")
 )

(test-suite
 "date- and timestamp-related functions"
 (is (clause-convert-epoch->timestamp)
     "SELECT timestamp 'epoch' + INTERVAL '1 second' * $1"
     "got correct string back for no params")

 (is (clause-convert-epoch->timestamp 3)
     "SELECT timestamp 'epoch' + INTERVAL '1 second' * $3"
     "got correct string back for one param")

 (is (clause-convert-epoch->timestamp #:subquery #t)
     "(SELECT timestamp 'epoch' + INTERVAL '1 second' * $1)"
     "got correct string back for subquery")
 )
