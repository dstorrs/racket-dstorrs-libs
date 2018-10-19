#lang at-exp racket/base

(require racket/contract/base
         racket/contract/region
         racket/file
         racket/format
         "utils.rkt")

(provide watch-dir-or-tree)

;;======================================================================
;;    This library does file system monitoring
;;======================================================================

;;    (define/contract (watch-dir-or-tree dir handler [handler-arg #f]
;;                                        #:poll-delay [poll-delay #f]
;;                                        #:recursive [recursive #t]
;;                                        #:pre [preprocessor #f])
;;
;; Keep an eye on a directory or on the directory and the entire
;; subtree below it.  If anything changes, run a handler.
;;
;; dir : The directory to watch.  If #:recursive is true (the default)
;; then it will watch the subtree below the specified directory in
;; addition to the directory itself.
;;
;; handler : The function to call with there is a change.  It will
;; receive a path to the changed directory plus the value of
;; 'handler-arg' (which defaults to #f)
;;
;; handler-arg : Anything you want.  Will be passed to the handler
;; when there is a change.  Not a good idea to pass a port or a db
;; connection, since you don't know how long it will be before the
;; thing is accessed and the port/connection/whatever might have gone
;; stale in that time.  Instead, pass a function that creates the
;; port/connection/whatever.
;;
;; #:poll-delay : Number of seconds to sleep between looking for
;; change events.  Defaults to 0, which means "don't actually sleep,
;; but do suggest to Racket that this is a good time to run other
;; threads if there are any waiting."
;;
;; #:recursive : Default #t.  If true, watch the entire subtree below the specified directory
;;
;; #:pre : Function that accepts one argument, a path-string?  If
;; specified, it will be called on once for each directory that is
;; going to be watched before the actual watching starts.  This is
;; good for, e.g., adding the path to the DB or otherwise recording
;; what you're going to be watching.
(define/contract (watch-dir-or-tree dir handler [handler-arg #f]
                                    #:poll-delay [poll-delay 0]
                                    #:recursive [recursive #t]
                                    #:pre [preprocessor #f])
  (->* (path-string? (-> path-string? any/c any))
       (any/c
        #:poll-delay exact-nonnegative-integer?
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
      (sleep poll-delay)
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
