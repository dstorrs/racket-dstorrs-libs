#lang racket

(require dstorrs/utils)

(define (threaded thnk [name "<unnamed>"])
  (->* ((-> any)) (any/c) any)

  (define thread-label (~a (rand-val "thread-") ": "))
  (say "about to spin off thread '" thread-label " to do " name)
  (thread thnk))
