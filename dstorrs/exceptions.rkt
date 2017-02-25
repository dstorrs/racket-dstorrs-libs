#lang at-exp racket

(provide (all-defined-out))

(define/contract (create-exn ctor msg . args)
  (->* (procedure? string?) () #:rest (listof any/c) exn?)
  (apply ctor (append (list msg (current-continuation-marks)) args)))



(define/contract (create/raise-exn ctor msg . args)
  (->* (procedure? string?) () #:rest (listof any/c) any)
  (raise (apply create-exn (append (list ctor msg) args))))



(struct exn:fail:num-rows exn:fail (expected got) #:transparent)
(struct exn:fail:num-rows:zero exn:fail:num-rows () #:transparent)
(struct exn:fail:num-rows:many exn:fail:num-rows () #:transparent)

(define/contract (refine-db-exn e)
  (-> exn? exn?)

  (define msg (exn-message e))

  (define wrong-number-of-rows (pregexp
                                @~a{returned wrong number of rows.+?expected:\s+(\d+).+?got:\s+(\d+)}
                                ))


  (define num string->number) ;; for convenience

  (match msg
    [(regexp wrong-number-of-rows (list _ expected got))
     (create-exn (if (= (num got) 0)
                     exn:fail:num-rows:zero
                     exn:fail:num-rows:many
                     )
                 msg (num expected) (num got))]

    [_ e])
  )
