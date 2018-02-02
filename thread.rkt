#lang racket

(require dstorrs/utils)

(provide execute-thunk
         threaded
         )

;;======================================================================
;;    Functions for conveniently threading your code.
;;======================================================================

;  (define/contract (execute-thunk thnk  #:async? [async? #f])
;
; Run a thunk.  If you set #:async? #t then it will be run in another
; thread via 'threaded' (see below)
(define/contract (execute-thunk thnk  #:async? [async? #f])
  (->* ((-> any)) ( #:async? boolean?) any)
  (if async?
      (threaded thnk)
      (thnk)))

;;----------------------------------------------------------------------

; (define/contract (threaded thnk)
;
; Run a thunk in a new thread.  The thread will be given a random
; label and all 'say' calls inside the thread will be prefixed with
; this label.
(define/contract (threaded thnk)
  (-> (-> any) any)

  (define thread-label (~a (rand-val "thread") ": "))
  (parameterize ((prefix-for-say (~a (prefix-for-say) " " thread-label)))
    (thread (thunk (begin0 (thnk) (sleep 0)))))) ; the sleep 0 suggests other threads should run
