#lang at-exp racket

(require dstorrs/try
         dstorrs/utils
         )

;;----------------------------------------------------------------------

(define/contract (watch-dir-or-tree dir handler [handler-arg #f] #:recursive [recursive #t] #:pre [preprocessor #f])
  (->* (path-string? (-> path-string? any/c any))
       (any/c #:recursive boolean? #:pre (-> path-string? any))
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
      (loop)))

  (void (thread work))
  )

(provide (all-defined-out))
