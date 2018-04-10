#lang racket

(require file/gzip
         file/gunzip
         handy/utils
         )

(provide gzip*
         gunzip*
         )

;; (define/contract (gzip* in-file [out-file #f] #:remove-original? [remove-original? #f])
;;    (->*  (path-string?) (path-string? #:remove-original? boolean?) path-string?)
;;
;; Most of the time, when you want to compress a file you want to know
;; where it ended up.  That's fine if you're specifying the path but
;; not so much if it's defaulting.  This calls gzip and then returns
;; the path where the file ended up.  If you set remove-original? to a
;; true value then the source file will be deleted after the
;; compression.
;;
(define/contract (gzip* in-file
                        [out-file #f]
                        #:remove-original? [remove-original? #f])
  (->*  (path-string?)
        (path-string?
         #:remove-original? boolean?)
        path-string?)

  (define out-filepath (or out-file (path-add-extension in-file ".gz" #".")))
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

  (gunzip in-file path-maker)

  (and remove-zip? (file-exists? out-filepath) (delete-file-if-exists in-file))
  out-filepath)
