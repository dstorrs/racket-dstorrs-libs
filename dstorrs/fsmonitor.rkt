#lang at-exp racket

(require dstorrs/try
         dstorrs/utils
         )

;;----------------------------------------------------------------------

(define/contract (watch-dir-or-tree dir handler [handler-arg #f]
                                    #:poll-delay [poll-delay #f]
                                    #:recursive [recursive #t]
                                    #:pre [preprocessor #f])
  (->* (path-string? (-> path-string? any/c any))
       (any/c #:poll-delay exact-nonnegative-integer?
              #:recursive boolean?
              #:pre (-> path-string? any))
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

  (when preprocessor
    (for ((item dirs-to-watch))
      (preprocessor item)))

  (define (evt-maker dir)
    (handle-evt (filesystem-change-evt dir)
                (lambda (e)
                  (handler dir handler-arg))))

  (define (work)
    (let loop ()
      (apply sync (map evt-maker dirs-to-watch))
      (when poll-delay
        (sleep poll-delay))
      (loop)))

  (define current-prefix (regexp-replace #px":\\s*$" (prefix-for-say) ""))
  (define thread-label (~a (rand-val "thread")
                           (if (empty-string? current-prefix)
                               ""
                               (~a " / " current-prefix))
                           ": "))
  (say "about to start fsmonitor thread with label: " thread-label)
  (parameterize ((prefix-for-say thread-label))
    (void (thread work))
    )
  )

(provide (all-defined-out))
