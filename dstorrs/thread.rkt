#lang racket

(require dstorrs/utils)

(provide execute-thunk
         threaded
         )

;;----------------------------------------------------------------------

(define/contract (execute-thunk thnk  #:async? [async? #f])
  (->* ((-> any)) ( #:async? boolean?) any)
  (if async?
      (threaded thnk)
      (thnk)))

;;----------------------------------------------------------------------

(define/contract (threaded thnk)
  (-> (-> any) any)

  (define thread-label (~a (rand-val "thread") ": "))
  (parameterize ((prefix-for-say thread-label))
    (thread (thunk (begin0 (thnk) (sleep 0)))))) ; the sleep 0 suggests other threads should run
