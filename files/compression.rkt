#lang racket/base

(require (for-syntax racket/base)
         file/gunzip
         file/gzip
         handy/try
         handy/utils
         racket/contract/base
         racket/contract/region)

(provide gzip*
         gunzip*
         (struct-out exn:fail:gunzip)
         (all-from-out file/gzip file/gunzip))

(struct exn:fail:gunzip exn:fail ())

;; (define/contract (gzip* in-file
;;                         [out-file #f]
;;                         #:remove-original? [remove-original? #f]
;;                         #:exists           [exists-flag 'error])
;;   (->*  (path-string?)
;;         (path-string?
;;          #:remove-original? boolean?
;;          #:exists (or/c 'error 'truncate))
;;         path-string?)
;;
;; file/gzip offers the 'gzip' function to compress a file with the
;; gzip algorithm, defaulting the output filepath if necessary.  gzip*
;; wraps it and offers two additional value points:
;;
;;    1) gzip* will not by default overwrite an existing file
;;    2) gzip* returns the output filepath
;;
;; Most of the time, when you want to compress a file you want to know
;; where it ended up.  That's fine if you're specifying the path but
;; not so much if it's defaulting.  This calls gzip and then returns
;; the path where the file ended up.  If you set remove-original? to a
;; true value then the source file will be deleted after the
;; compression.
;;
;; If you specify the out-file, great.  If you don't then it defaults
;; to in-file + ".gz".
;;
;; If the destination file exists then the behavior is determined by
;; the value passed to #:exists.
;;     'error     (DEFAULT) means that it will throw exn:fail:filesystem.
;;     'truncate  means that it will overwrite the contents of the file
;;
;; As long as you use the default 'error flag, you can be confident that two separate threads will not 
(define/contract (gzip* in-file
                        [out-file #f]
                        #:remove-original? [remove-original? #f]
                        #:exists           [exists-flag 'error])
  (->*  (path-string?)
        (path-string?
         #:remove-original? boolean?
         #:exists (or/c 'error 'truncate))
        path-string?)

  (define out-filepath (or out-file (path-add-extension in-file ".gz" #".")))

  ; create or truncate the file that we will compress to.  By default
  ; the exists-flag is 'error, which means that if the already exists
  ; then this will thrown exn:fail:filesystem.  This ensures that two
  ; instances of gzip* cannot stomp each other via race condition.
  ;
  ; You can use #:exists 'truncate if you want to overwrite an
  ; existing file, but that opens you up to race conditions if two
  ; threads are attempting to gzip to the same file.
  ;
  (open-output-file out-filepath #:exists exists-flag)

  ; If we get to here then we didn't blow up, so it's okay to do the
  ; compression, which will overwrite the placeholder file that we
  ; created via open-output-file
  (gzip in-file out-filepath)

  (and remove-original? (file-exists? out-filepath) (delete-file in-file))
  out-filepath)

;;----------------------------------------------------------------------

;; (define/contract (gunzip* in-file [output-path-or-maker #f] #:remove-zip? [remove #t])
;;   (->*  (path-string?)
;;          ; you can specify the output path or a func to produce it
;;          ((or/c path-string? (-> string? boolean? path-string?))
;;           #:remove-zip? boolean?)
;;          path-string?)
;;
;; The counterpart of gzip*, this calls gunzip and then returns the
;; path where the uncompressed file ended up.  If you want to specify
;; the output path you can either provide a path-string? directly or
;; provide a function to generate it.
;;
;; The procedure to calculate the destination is applied to two
;; arguments--the default destination file name and a boolean that is
;; #t if this name was read from file--before the destination file is
;; created. The return value of the file is used as the actual
;; destination file name (to be opened with the 'truncate flag of
;; open-output-file).
;;
;; NOTE THAT BY DEFAULT THE ZIP FILE IS REMOVED AFTER DECOMPRESSION,
;; although only if the output file was created.
;;
(define/contract (gunzip* in-file [output-path-or-maker #f] #:remove-zip? [remove-zip? #t])
  (->*  (path-string?)
        ; you can specify the output path or a func to produce it
        ((or/c path-string? (-> path-string? boolean? path-string?))
         #:remove-zip? boolean?)
        path-string?)


  (define-values (out-dir ignore1 ignore2) (split-path in-file))
  (define out-filepath (cond [(path-string? output-path-or-maker)
                              output-path-or-maker]
                             [else
                              #f]))
  (define path-maker
    (cond [(path-string? output-path-or-maker)
           (lambda (x y) output-path-or-maker)]
          [(procedure? output-path-or-maker)
           (lambda (file archive-supplied?)
             (define result (path-string->string (output-path-or-maker file archive-supplied?)))
             (set! out-filepath result)
             result)]
          [else
           (lambda (file archive-supplied?)
             (define result  (build-path out-dir file))
             (set! out-filepath result)
             result)]))

  ;;  gunzip the file.  If the file is not a gzip or is corrupted then
  ;;  the default gunzip will raise an 'exn:fail'.  If that happens,
  ;;  re-raise it as an 'exn:fail:gunzip' with the same information.
  ;;  For any other kind of exception, re-raise it as is
  ;;
  (try [(gunzip in-file path-maker)]
       [catch (exn:fail? (lambda (e)
                           (define msg (exn-message e))
                           (raise (cond [(regexp-match #px"^gnu-unzip:" msg)
                                         (exn:fail:gunzip msg (exn-continuation-marks e))]
                                        [(regexp-match #px"^inflate: unexpected end-of-file" msg)
                                         (exn:fail:gunzip msg (exn-continuation-marks e))]
                                        [else e]))))])

  (and remove-zip? (file-exists? out-filepath) (delete-file-if-exists in-file))
  out-filepath)
