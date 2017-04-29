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
     "timestamp 'epoch' + INTERVAL '1 second' * $1"
     "clause-convert-epoch->timestamp: got correct string back for no params")

 (is (clause-convert-epoch->timestamp #:complete #t)
     "SELECT timestamp 'epoch' + INTERVAL '1 second' * $1"
     "clause-convert-epoch->timestamp: got correct string back for no params with #:complete")

 (is (clause-convert-epoch->timestamp 3)
     "timestamp 'epoch' + INTERVAL '1 second' * $3"
     "clause-convert-epoch->timestamp: got correct string back for one param")

 (is (clause-convert-epoch->timestamp 3 #:complete #t)
     "SELECT timestamp 'epoch' + INTERVAL '1 second' * $3"
     "clause-convert-epoch->timestamp: got correct string back for one param with #:complete")

 (is (clause-convert-epoch->timestamp #:subquery #t)
     "(SELECT timestamp 'epoch' + INTERVAL '1 second' * $1)"
     "clause-convert-epoch->timestamp: got correct string back for subquery")

 (is (clause-convert-epoch->timestamp #:subquery #t #:complete #f)
     "(SELECT timestamp 'epoch' + INTERVAL '1 second' * $1)"
     "clause-convert-epoch->timestamp: got correct string back for subquery with #:complete #f")

 (is (clause-convert-timestamp->epoch)
     "extract('epoch' from $1)"
     "clause-convert-timestamp->epoch: got correct string back for no param")

 (is (clause-convert-timestamp->epoch #:complete #t)
     "SELECT extract('epoch' from $1)"
     "clause-convert-timestamp->epoch: got correct string back for no param with #:complete")

 (is (clause-convert-timestamp->epoch 3)
     "extract('epoch' from $3)"
     "clause-convert-timestamp->epoch: got correct string back for integer param")

 (is (clause-convert-timestamp->epoch 3 #:complete #t)
     "SELECT extract('epoch' from $3)"
     "clause-convert-timestamp->epoch: got correct string back for integer param with #:complete")

 (is (clause-convert-timestamp->epoch (clause-convert-epoch->timestamp #:subquery #t))
     "extract('epoch' from (SELECT timestamp 'epoch' + INTERVAL '1 second' * $1))"
     "clause-convert-timestamp->epoch: got correct string back for string param")

 (is (clause-convert-timestamp->epoch #:complete #t (clause-convert-epoch->timestamp #:subquery #t))
     "SELECT extract('epoch' from (SELECT timestamp 'epoch' + INTERVAL '1 second' * $1))"
     "clause-convert-timestamp->epoch: got correct string back for string param with #:complete")


 )
