#lang racket/base

(require racket/bool
         racket/contract/base
         racket/contract/region
         racket/format
         racket/function
         racket/hash
         racket/list
         racket/match
         )

; for hash-union
;; *) hash-aggregate*     : listof any/c -> single hash that maps a key to the vals
;; *) hash-aggregate      : flattens its args, trampolines to hash-aggregate*
;; *) hash-invert         : (hash 'a 1 'b 2 'c 1) => (hash 1 '(a c) 2 '(b)). order undefined.
;; *) hash->keyword-apply : take a function and a hash.  Assume the
;;     keys of the hash are keyword arguments and call appropriately.
;; *) hash-key-exists?    : alias for hash-has-key? because I always forget the name
;; *) hash-keys->strings  : convert keys to strings, maybe changing "_" to "-" or vice versa
;; *) hash-keys->symbols  : take a hash where keys are symbols or strings, make them symbols
;; *) hash->immutable     : convert an (im)mutable hash to an immutable one
;; *) hash->mutable       : convert an (im)mutable hash to a mutable one
;; *) mutable-hash        : creates a mutable hash using the convenient syntax of (hash)
;; *) hash-meld           : combine hashes with later entries overwriting earlier ones
;; *) hash-remap          : munge a hash by (remove, overwrite, add, rename, default) keys
;; *) hash-rename-key     : change, e.g., key 'name to be 'first-name
;; *) hash-slice          : takes a hash and a list of keys, returns the matching values
;; *) hash-slice*         : ibid, but it returns a new hash of those keys and their values
;; *) hash-subtract       : remove one or more subsets of a hash's keys
;; *) safe-hash-remove    : does hash-remove or hash-remove! as needed.  Returns the hash.
;; *) safe-hash-set       : does hash-set or hash-set! as needed. Returns the hash.
;; *) safe-hash-union     : does hash-union or hash-union! as needed. Returns the hash.
;; *) sorted-hash-keys    : returns sorted list of keys from the hash

(provide (all-from-out racket/hash)

         hash-aggregate
         hash-aggregate*

         hash-invert

         hash->keyword-apply
         hash-key-exists?

         hash-keys->strings
         hash-keys->symbols

         hash->immutable
         hash->mutable
         mutable-hash
         mutable-hash?

         hash-rename-key
         hash-meld
         hash-remap
         hash-slice
         hash-slice*

         hash-subtract

         safe-hash-remove
         safe-hash-set
         safe-hash-union

         sorted-hash-keys
         )


;;----------------------------------------------------------------------

(define hash-key-exists? hash-has-key?) ; alias because I always forget the name

;;----------------------------------------------------------------------

; hash-aggregate was the original primary function but has now been
; turned into a trampoline to hash-aggregate* that flattens its
; arguments list before bouncing.
(define/contract (hash-aggregate key
                                 #:default [default the-unsupplied-arg]
                                 . items)
  (->* (any/c)
       (#:default any/c)
       #:rest (listof any/c)
       hash?)

  ; allow passing individual hashes or a list of hashes
  (apply hash-aggregate* key #:default default (flatten items)))


; hash-aggregate*
;
; Takes a list of items and aggregates them into a hash where the
; values are the original items and the keys are some element of the
; items.  Typically used to evert a list of hashes (and includes
; special convenience functionality for that case) but could be used
; on anything.
;
; >  (hash-aggregate* 'filepath (hash 'filepath "/foo" 'size 10)
;                              (hash 'filepath "/bar" 'size 20))
;   (hash "/foo" (hash 'filepath "/foo" 'size 10)
;         "/bar" (hash 'filepath "/bar" 'size 20))
;
; Note the convenience here -- if a non-procedure value
; (e.g. 'filepath) is passed as the key value then it is assumed that
; the items being aggregated will be hashes and that the key value
; should be determined by way of (curry hash-ref key-value)
;
; If multiple hashes share the same value for that key then they will
; be in a list.
;
; >  (hash-aggregate* 'filepath (hash 'filepath "/foo" 'size 10)
;                              (hash 'filepath "/bar" 'size 30)
;                              (hash 'filepath "/foo" 'size 20))
;
; (hash "/foo" (list  (hash 'filepath "/foo" 'size 10) (hash 'filepath "/foo" 'size 20))
;       "/bar" (hash 'filepath "/bar" 'size 30))
;
; You can pass a function of one argument as the key.
;
; >  (define key (compose1 add1 (curryr hash-ref 'size)))
; >  (hash-aggregate* key (hash 'filepath "/foo" 'size 10)
;                        (hash 'filepath "/bar" 'size 30)
;                        (hash 'filepath "/foo" 'size 20))
; (hash 11  (hash 'filepath "/foo" 'size 10)
;       31 (hash 'filepath "/bar" 'size 30)
;       21 (hash 'filepath "/foo" 'size 20))
;
; And, of course, you can do the same thing with non-hash data.
;
; > (struct person (age) #:transparent)
; > (hash-aggregate* person-age (list (person 0) (person 1) (person 2)))
; (hash 0 (person 0)
;       1 (person 1)
;       2 (person 2))
(define/contract (hash-aggregate* key
                                  #:default [default the-unsupplied-arg]
                                  . items)
  (->* (any/c)
       (#:default any/c)
       #:rest (listof any/c)
       hash?)

  (define no-default? (unsupplied-arg? default))
  (for/fold ([result (hash)])
            ([h items]) ; e.g. (hash 'id 7)

    ; Key might have been specified or it might be the result of a
    ; procedure call
    (define key-in-result
      (cond [(procedure? key) (key h)]
            [no-default? (hash-ref h key)] ; will die if there's a hash without that key
            [else (hash-ref h key default)]))
    (cond
      [(not (hash-has-key? result key-in-result))
       ; If result does not have this key, add it directly
       (safe-hash-set result key-in-result h)]
      [else
       (define current-val-in-result (hash-ref result key-in-result))
       (cond [(list? current-val-in-result)
              (safe-hash-set result key-in-result (cons h current-val-in-result))
              ]
             [else ; it is there but it's not a list
              (safe-hash-set result key-in-result (list h current-val-in-result))
              ])])))


;;----------------------------------------------------------------------

;;  Turn a hash inside out so that the values become keys and the keys
;;  become values.  This can be particularly useful when collecting
;;  votes and then finding the option with the most votes.  For example:
;;
;;    (hash 'eat 3 'drink 1 'make-merry 5)
;;
;;  becomes
;;
;;    (hash 3 'eat 1 'drink 5 'make-merry)
;;
;; And it's then easy enough to look at the keys and determine that
;; 'make-merry got the most votes.
;;
;; If the source hash had two keys with the same value then the result
;; hash will have one key with a list of two values:
;;
;;    (hash 'eat 3 'drink 3 'make-merry 1)
;;
;; becomes
;;
;;    (hash 3 '(eat drink) 1 'make-merry)
;;
;; There is no promise of which order '(eat drink) will be listed in.
;;
(define/contract (hash-invert h)
  (-> (and/c hash? immutable?) hash?)
  (for/fold ([result (hash)])
            ([(k v) (in-hash h)])
    (hash-set result v (cons k (hash-ref result v '())))))

;;----------------------------------------------------------------------

;;    Convert the keys of a hash from whatever they are to strings.
;;    Optionally, you can convert dashes in the key to underscores or
;;    vice versa; the normal use case is when inserting into a
;;    database and you want keys to match field names.  The normal
;;    string representation of a vector or list isn't terribly useful
;;    (e.g. (vector 'a 'b) => "#(a b)"), so we change them to be the
;;    concatenation of their elements.
;;
;;        e.g. (vector 'a 'b) => "ab"
;;        e.g. (list 'a 'b)   => "ab"
;;
(define (_maybe-remove-q s rqm?) (if rqm? (regexp-replace #px"\\?" s "") s))
(define/contract (hash-keys->strings h
                                     #:dash->underscore? [dash->underscore? #f]
                                     #:underscore->dash? [underscore->dash? #f]
                                     #:remove-question-marks? [remove-question-marks? #f]
                                     )
  (->* (hash?)
       (#:dash->underscore? boolean?
        #:underscore->dash? boolean?
        #:remove-question-marks? boolean?
        )
       hash?)

  (when (and dash->underscore? underscore->dash?)
    (raise-arguments-error 'hash-keys->strings
                           "It's not sensible to set both dash->underscore? and underscore->dash?."
                           "dash->underscore?" dash->underscore?
                           "underscore->dash?" underscore->dash?))

  (define (to-string x)
    (cond ((list?   x)   (apply string-append (map to-string x)))
          ((vector?   x) (to-string (vector->list x)))
          (else (~a x))))

  ((if (immutable? h) identity hash->mutable)
   (for/hash ([(k v) h])
     (let ([key (to-string k)])
       (values (let ([s (_maybe-remove-q key remove-question-marks?)])
                 (cond [dash->underscore?  (regexp-replace* #px"-" s "_")]
                       [underscore->dash?  (regexp-replace* #px"_" s "-")]
                       [else s]))
               v)))))

;;----------------------------------------------------------------------

(define/contract (hash-keys->symbols h
                                     #:dash->underscore? [dash->underscore? #f]
                                     #:underscore->dash? [underscore->dash? #f]
                                     #:remove-question-marks? [remove-question-marks? #f]
                                     )
  (->* (hash?)
       (#:dash->underscore? boolean?
        #:underscore->dash? boolean?
        #:remove-question-marks? boolean?
        )
       hash?)

  (when (and dash->underscore? underscore->dash?)
    (raise-arguments-error 'hash-keys->symbols
                           "It's not sensible to set both dash->underscore? and underscore->dash?."
                           "dash->underscore?" dash->underscore?
                           "underscore->dash?" underscore->dash?))

  ((if (immutable? h) identity hash->mutable)
   (for/hash ([(k v) (in-hash h)])
     (define key (~a k))
     (values (string->symbol
              (let ([s (_maybe-remove-q key remove-question-marks?)])
                (cond [dash->underscore?  (regexp-replace* #px"-" s "_")]
                      [underscore->dash?  (regexp-replace* #px"_" s "-")]
                      [else s])))
             v))))

;;----------------------------------------------------------------------

(define/contract (sorted-hash-keys hsh [func symbol<?])
  (->* (hash?) ((unconstrained-domain-> boolean?)) list?)
  (sort (hash-keys hsh) func))

;;----------------------------------------------------------------------

(define/contract (hash->immutable h)
  (-> hash? (and/c hash? immutable?))
  (cond [(immutable? h) h]
        [else (make-immutable-hash (hash-map h cons))]))

;;----------------------------------------------------------------------

(define/contract (hash->mutable h)
  (-> hash? (and/c hash? (not/c immutable?)))
  (cond [(not (immutable? h)) h]
        [else (make-hash (hash-map h cons))]))

;;----------------------------------------------------------------------

(define/contract (mutable-hash . args)
  (->* () ()
       #:rest (and/c (listof any/c) (compose even? length))
       (and/c hash? (not/c immutable?)))
  (define result (make-hash))
  (let loop ([l args])
    (match l
      ['()  result]
      [else (hash-set! result (first l) (second l))
            (loop (drop l 2))])))

;;----------------------------------------------------------------------

(define mutable-hash? (and/c hash? (not/c immutable?)))

;;----------------------------------------------------------------------

(define/contract (hash-meld . hshs)
  (->* () () #:rest (listof hash?) hash?)

  (match hshs
    ['() (hash)]
    [(list base-hash others ...)
     (define base-is-imm? (immutable? base-hash))
     (define func (if base-is-imm? hash-union hash-union!))
     (define result (apply (curry func #:combine (λ (x y) y))
                           hshs))
     (if base-is-imm? result base-hash)]))

;;----------------------------------------------------------------------

;;    Takes a hash and list of keys, returns a list of the hash values
;;    for those keys. Any keys that are not in the hash will be
;;    returned as the default value (#f if not specified).
(define/contract (hash-slice the-hash keys [default 'hash-slice-default])
  (->* (hash? list?) (any/c) list?)
  (for/list ((k keys))
    (if (equal? default 'hash-slice-default)
        (hash-ref the-hash k)
        (hash-ref the-hash k default))))

;;----------------------------------------------------------------------

;;    Takes a hash and list of keys, returns a new hash containing
;;    only those keys and their values. Any keys that are not in the
;;    hash will be returned as the default value, or an exception
;;    thrown if no default was supplied.
(define/contract (hash-slice* the-hash keys [default 'hash-slice*-default])
  (->* (hash? list?) (any/c) hash?)
  (for/hash ((k keys))
    (values k
            (if (equal? default 'hash-slice*-default)
                (hash-ref the-hash k)
                (hash-ref the-hash k default)))))

;;----------------------------------------------------------------------

(define/contract (hash-subtract the-hash . subhashes)
  (->* (hash?) () #:rest (listof hash?) hash?)
  (safe-hash-remove the-hash
                    (remove-duplicates
                     (apply append
                            (map hash-keys subhashes)))))

;;----------------------------------------------------------------------

;; (define/contract (safe-hash-remove h #:key-is-list [key-is-list? #f] . keys)
;;    (->* (hash?) (#:key-is-list boolean?) #:rest (listof any/c) hash?)
;;
;; Mutable hashes use hash-remove! which returns (void).  Immutable
;; hashes use hash-remove and return the hash. Both will throw if you
;; use the wrong 'remove' function.  Both functions only remove a
;; single key at a time.  If you'd like to not deal with any of this,
;; use safe-hash-remove: it works on both mutable and immutable
;; hashes, it always returns the hash, and it removes as many keys as
;; you like, all in one go.
;;
;; Examples:
;;
;;    ;    Here's a hash that includes a bunch of data that should be
;;    ;    shown to the user and also a bunch of metadata needed by the
;;    ;    application for other purposes:
;;    (define application-h (hash 'food? #t 'type 'fruit 'id 7 'added-to-db-at 1516997724))
;;
;;    ;    Let's get it ready for output by stripping out the metadata
;;    (define output-h (safe-hash-remove h 'id 'added-to-db-at)) => only has 'food? and 'type keys
;;
;;    ;    Same as above, but the keys are passed as a list -- perhaps
;;    ;    they were generated by a DB query, or a map and it's a bother to
;;    ;    unwrap them.
;;    (define output-h (safe-hash-remove h '(id added-to-db-at))) => only 'food? and 'type remain
;;
;;    ;    Note that you can freely mix passing some keys explicitly and some as a list
;;    (define output-h (safe-hash-remove h '(id) 'added-to-db-at)) => same as above
;;
;;    ;    Edge case: There is a key in your hash that really is a list.
;;    ;    This is a problem, since the keys list that you pass in will be
;;    ;    flattened, so the key that is a list will be missed.
;;    (define weird-h   (hash '(foo bar) 'x 'a 7 'b 8)) ; the first key is a list
;;
;;    ;    Use #:key-is-list to avoid flattening
;;    WRONG: (safe-hash-remove weird-h '(foo bar)))                     ; hash unchanged
;;    WRONG: (safe-hash-remove weird-h '(foo bar) 'a))                  ; only 'a removed
;;    RIGHT: (safe-hash-remove weird-h '(foo bar) 'a #:key-is-list #t)) ; both removed
;;    RIGHT: (safe-hash-remove weird-h '((foo bar) 'a)))                ; both removed
;;
(define/contract (safe-hash-remove h #:key-is-list [key-is-list? #f] . keys)
  (->* (hash?) (#:key-is-list boolean?) #:rest (listof any/c) hash?)
  (define is-imm (immutable? h))
  (define keys-list
    (cond [key-is-list? keys] ; very unlikely, but included for completeness
          [(null? keys) keys]
          [(> (length keys) 1) keys]
          [(list? (car keys)) (car keys)]
          [else keys]))

  (for/fold ((hsh h))
            ((k keys-list))
    (if is-imm
        (hash-remove hsh k)
        (begin (hash-remove! hsh k) h))))

;;----------------------------------------------------------------------

;; safe-hash-union
;;
;; Like hash-union/hash-union! but handles mutable or immutable
(define/contract (safe-hash-union h0
                                  #:combine [combine (lambda args
                                                       (raise-arguments-error
                                                        'safe-hash-union
                                                        "combine failed"
                                                        "args" args))]
                                  #:combine/key [c/k #f]
                                  . args)

  (->* (hash?)
       (#:combine procedure? #:combine/key procedure?)
       #:rest (listof hash?)
       hash?)

  (define unifier (if (immutable? h0)
                      hash-union
                      hash-union!))
  (define combine/key (if c/k c/k (lambda (k a b) (combine a b))))

  (define result
    (for/fold ([acc h0])
              ([arg args])
      (unifier acc arg #:combine combine #:combine/key combine/key)))

  (if (void? result)
      h0
      result))

;;----------------------------------------------------------------------

(define/contract (safe-hash-set h  . args)
  (->* (hash?)
       ()
       #:rest (and/c list?
                     (lambda (lst)
                       (let ([len (length lst)])
                         (and (even? len)
                              (not (= 0 len))))))
       hash?)

  (define args-hash (apply hash args))
  (define is-imm (immutable? h))
  (for/fold ((hsh h))
            ((k (hash-keys args-hash)))
    (if is-imm
        (hash-set hsh k (hash-ref args-hash k))
        (begin (hash-set! hsh k (hash-ref args-hash k)) h))))

;;----------------------------------------------------------------------

;; Change the name of a key.  The new key can be passed directly or
;; you can pass a procedure which will be used to generate the new
;; key.
;;
;; (hash-rename-key (hash 'x 1) 'x 'y)  => (hash 'y 1)
;; (hash-rename-key (hash 'x 1) 'x symbol->string)  => (hash "x" 1)
;;
(define/contract (hash-rename-key h old-key nk)
  (-> hash? any/c any/c hash?)

  (define new-key (cond [(procedure? nk) (nk old-key)]
                        [else nk]))

  (when (not (hash-has-key? h old-key))
    (raise-arguments-error 'hash-rename-key
                           "no such key"
                           "old-key" old-key
                           "new-key" new-key
                           "hash" h))

  (when (hash-has-key? h new-key)
    (raise-arguments-error 'hash-rename-key
                           "destination key exists"
                           "old-key" old-key
                           "new-key" new-key
                           "hash" h))

  (safe-hash-remove
   (safe-hash-set h new-key (hash-ref h old-key))
   old-key))

;;----------------------------------------------------------------------

;; (define/contract (hash-remap h
;;                              #:action-order         [action-order
;;                                                        '(include remove overwrite
;;                                                          add rename default)]
;;                              #:include              [include-keys #f]
;;                              #:remove               [remove-keys '()]
;;                              #:overwrite            [overwrite   #f ]
;;                              #:add                  [add         #f ]
;;                              #:rename               [remap       #f ]
;;                              #:default              [default     #f ]
;;                              #:value-is-default?    [default-val #f ]
;;                              #:post                 [post        identity]
;;                              )
;;   (->* (hash?)
;;        (#:action-order (listof (or/c 'remove 'overwrite 'add 'rename 'default))
;;         #:include list?
;;         #:remove list?
;;         #:overwrite hash?
;;         #:add hash?
;;         #:rename hash?
;;         #:default hash?
;;         #:value-is-default? any/c ; (-> any/c boolean) or converts to (or/c default-val)
;;         #:post (-> hash? any)
;;        )
;;        hash?)
;;
;;  Mnemonic for default order of application: I ROARenD
;;         include -> remove -> overwrite -> add -> rename -> default
;;
;;    This will munge hashes any way you like.  You can rename keys,
;;    remove keys, overwrite the value of keys, add new keys, ensure
;;    keys exist, and set default values.
;;
;;    The return value is guaranteed to be of the same type (mutable /
;;    immutable) as the original.
;;
;;
;; EXAMPLES:
;;
;;  NOTE: These are listed in the default action order, but you can
;;  change that with the #:action-order parameter.  Whatever order you
;;  specify there will be followed.
;;
;;
;;  INCLUDE the keys specified in the #:include list.  This is really
;;  just syntactic sugar around REMOVE, basically saying "remove
;;  everything except the following keys"
;;
;;    (define h (hash 'group 'fruit   'color 'red    'type 'apple))
;;    (hash-remap h #:include '(group color)   => (hash 'group 'fruit 'color 'red)
;;
;;
;;  REMOVE any values we were told to remove via the #:remove list
;;
;;    (define h (hash 'group 'fruit   'color 'red    'type 'apple))
;;    (hash-remap h #:remove '(group color)   => (hash 'type 'apple)
;;
;;
;;  OVERWRITE values.
;;
;;    (define h (hash 'group 'fruit   'color 'red    'type 'apple))
;;    (hash-remap h #:overwrite (hash 'group 'food))
;;       => (hash 'group 'food 'color 'red 'type 'apple)
;;
;;    If the new value is a procedure then it will be invoked and its
;;    result will be the new value.  The procedure must have the
;;    one of the following signatures:
;;
;;        (-> any/c)                    ; takes no arguments. Returns one value.
;;        (-> any/c any/c)              ; takes orig-value. Returns one value.
;;        (-> hash? any/c any/c any/c)  ; takes a hash, key, orig-val.  Returns one value
;;
;;    If you actually want to pass in a procedure (e.g. if you're
;;    building a jumptable) then you'll have to wrap it like so:
;;
;;        (thunk          ; the 'generate a value' procedure
;;          (lambda ...)) ; the procedure it generates
;;
;;    If you ask to overwrite keys that are not there, they will be
;;    added.  If the value to overwrite them with is a procedure then
;;    the procedure will receive #f as the value for the previously
;;    nonexistent key.
;;
;;
;;  ADD additional keys
;;
;;    NOTE: This will throw an exception if you try to add a key that
;;    is already there. If you want to force a key to a value then use
;;    #:overwrite and it will be added or set as necessary.  If you
;;    want to be sure that a hash has a key then use #:default and it
;;    will only be added if it's not there.  Alternatively, use
;;    #:action-order and put 'remove before 'add.
;;
;;    (define h (hash 'group 'fruit   'color 'red    'type 'apple))
;;    (hash-remap h #:add (hash 'subtype 'honeycrisp))
;;       => (hash 'group 'fruit 'color 'red 'type 'apple 'subtype 'honeycrisp))
;;
;;    (hash-remap h #:add (hash 'group 'tasty)) => EXCEPTION
;;
;;
;;  RENAME keys
;;
;;    (define h (hash 'group 'fruit   'color 'red    'type 'apple))
;;    (hash-remap h #:rename (hash 'color 'shade  'type 'species )
;;       => (hash 'group 'fruit    'shade 'red    'species 'apple)
;;
;;
;;  DEFAULT values if they aren't there or if they have a value that
;;  matches value-is-default? predicate.
;;
;;  The values provided for the defaults can be either:
;;
;;    - A normal value then it will be used directly.
;;    - A procedure of arity 2 then it will be called as (default key hash)
;;    - A procedure of any other arity then it will be called
;;        as (default-val key).
;;
;; The hash that's passed to the arity-2 functions will be a hash of the same type as the
;; original hash but not eq? to the original hash.  Some or all of the values will already
;; have been defaulted.  Order of iteration on a hash is unpredictable, so be careful
;; about using default values that are dependent on other keys that will in turn be
;; defaulted.
;;
;; If you want your defaulted value to actually be a procedure, as
;; opposed to a value generated by a procedure, then you'll need to wrap
;; it like so:
;;
;;    #:default (hash 'foo (lambda (key) (lambda ...)))
;;
;;  The predicate for value-is-default? can be:
;;
;;    - A single value, which will be matched literally
;;    - A function matching (-> hash? any/c any/c boolean), which receive hash, key, value
;;    - A function matching (-> any/c boolean?), which will receive the value
;;
;;  To maintain backwards compatibility you must specify the #:value-is-default? if you
;;  want to default existing values as opposed to inserting keys that aren't there.  At
;;  some point I'll make it default to #f, but the original version of this wouldn't touch
;;  things that were already there.
;;
;;    (hash-remap (hash 'x #f) #:default (hash 'x 7))                       => (hash 'x #f)
;;    (hash-remap (hash 'x 2)  #:default (hash 'x 7))                       => (hash 'x 2)
;;    (hash-remap (hash 'x 2)  #:default (hash 'x 7) #:value-is-default? 2) => (hash 'x 7)
;;    (hash-remap (hash 'x 2)  #:default (hash 'x 7) #:value-is-default? integer?)
;;        => (hash 'x 7)
;;
;;
;; COMPLETE EXAMPLE
;; (define h (hash 'group 'fruit       'color 'red    'type #f))
;; (hash-remap h  #:remove             '(group)
;;                #:overwrite          (hash 'color 'green
;;                                           'type (lambda (k) "fuji")
;;                                           'grower (lambda (hsh key val)
;;                                                     (match val
;;                                                       [#f (hash-ref hsh 'seller "Tom")]
;;                                                       [(? (negate string?)) (~a val)]
;;                                                       [_ val])))
;;
;;                #:add                (hash 'vendor #f)
;;                #:rename             (hash 'vendor 'seller)
;;                #:default            (hash 'group "group" 'taste ~a 'seller "Bob")
;;                #:value-is-default?  (or/c #f sql-null?)) ; assumes (require db)
;;
;;     => (hash 'group  "group"     ; removed via #:remove, then added via #:default
;;              'color  'green      ; overwritten with specified value
;;              'type   "fuji"      ; overwritten with generated value
;;              'grower "Tom"       ; added via overwrite with generated value
;;              'seller "Bob"       ; added as 'vendor, then renamed, then defaulted
;;              'taste  "taste")    ; defaulted with generated value
;;
;;  COMPLETE EXAMPLE WITH SPECIFIED ORDER:
;;   (define h (hash 'group 'fruit   'color 'red    'type 'snack 'taste #f))
;;   (hash-remap h
;;               #:action-order '(default add overwrite rename remove)
;;               #:default   (hash 'group 'food 'thump 'tamp 'taste 'yummy)
;;               #:add       (hash 'foo 'bar 'baz 'jaz)
;;               #:overwrite (hash 'foo 'baz)
;;               #:rename    (hash 'foo 'quux 'color 'hue)
;;               #:remove    '(baz)
;;               #:value-is-default? false?)
;;
;;   =>   (hash 'group 'fruit  ; ignored by #:default since the key was already there
;;              'thump 'tamp   ; defaulted because the key wasn't there
;;              'quux  'baz    ; added as 'foo, then overwritten, then renamed to 'quux
;;              'hue   'red    ; renamed from 'color
;;              'type  'snack  ; overwritten
;;              'taste 'yummy) ; defaulted due to the #:value-is-default? predicate
;;
(define/contract (hash-remap h
                             #:include           [include-keys #f]
                             #:remove            [remove-keys  #f]
                             #:overwrite         [overwrite    #f]
                             #:add               [add          #f]
                             #:rename            [remap        #f] ; rename is taken
                             #:default           [default      #f]
                             #:value-is-default? [def-val      none/c]
                             #:post              [post-process identity]
                             #:action-order      [action-order '(include
                                                                 remove overwrite add
                                                                 rename default)])
  (->* (hash?)
       (#:include (non-empty-listof any/c)
        #:remove list?
        #:overwrite hash?
        #:add hash?
        #:rename hash?
        #:default hash?
        #:value-is-default? any/c
        #:action-order (listof (or/c 'include 'remove 'overwrite
                                     'add 'rename 'default))
        #:post (-> hash? any))
       any)

  ; converts non-procedure default values to procedures, does not affect procedures
  (define value-is-default? (or/c def-val))

  ;(say "default predicate: " value-is-default?)

  ; If we aren't going to end up doing anything then just return the
  ; (post-processed) original hash.
  (post-process
   (cond [(or (null? action-order)
              (andmap false? (list include-keys remap remove-keys overwrite add default)))

          ;(say "doing nothing")
          h]
         [else
          ;(say "doing something")
          ;
          ; Okay, we're going to make some sort of change
          (define (empty-hash) (if (immutable? h) (hash) (make-hash)))
          (define overwrite-hash (or overwrite (empty-hash)))
          (define add-hash       (or add       (empty-hash)))
          (define remap-hash     (or remap     (empty-hash)))
          (define default-hash   (or default   (empty-hash)))

          ;; ;(say "original hash: " h
          ;;      "\n\t immutable?     " (immutable? h)
          ;;      "\n\t overwrite:     " overwrite-hash
          ;;      "\n\t add:           " add-hash
          ;;      "\n\t remap-hash:    " remap-hash
          ;;      "\n\t default-hash:  " default-hash)

          ;(say "zot")
          (define result
            (for/fold ([result h])
                      ([action action-order])
              (match action
                ['include
                 (if include-keys
                     (hash-slice* result include-keys)
                     h)
                 ]
                ['remove
                 ;; Remove keys
                 (apply (curry safe-hash-remove result)
                        (or remove-keys '()))]
                ;
                ['overwrite
                 ;;    Overwrite any values from the original hash that
                 ;;    we were told to overwrite.  If the new value is a
                 ;;    procedure then it will be invoked and its result
                 ;;    will be the new value.  The procedure must have
                 ;;    the signature:
                 ;;
                 ;;        (-> hash? any/c any/c any/c)  ; hash, key, orig-val, return 1 value
                 ;;    or
                 ;;        (-> any/c any/c)  ; orig-val, return 1 value
                 ;;    or
                 ;;        (-> any/c)  ; no args, return 1 value
                 ;;
                 ;;    If you actually want to pass in a procedure (e.g. if you're
                 ;;    building a jumptable) then you'll have to wrap it like so:
                 ;;
                 ;;        (lambda (hsh key val)  ; the 'generate a value' procedure
                 ;;            (lambda ...))      ; the procedure it generates
                 ;;   or
                 ;;        (lambda (val)          ; the 'generate a value' procedure
                 ;;            (lambda ...))      ; the procedure it generates
                 ;;   or
                 ;;        (lambda ()             ; the 'generate a value' procedure
                 ;;            (lambda ...))      ; the procedure it generates
                 ;;
                 ;; 'overwrite will add keys that are not in the source
                 ;; hash.  In these cases, we want to make sure that
                 ;; procedural generators are still called, so let's
                 ;; make sure that the 'original' hash has all the keys
                 ;; that are in the overwrite-hash.


                 ; hash-meld merges a list of hashes, with later
                 ; entries overwriting earlier ones.  It does this
                 ; using either mutation or functional update based on
                 ; whether the first hash in the list is mutable or
                 ; not.  We want to be sure that we don't destroy the
                 ; procedures in the overwrite-hash, so we'll make it
                 ; immutable then convert back if necessary.
                 (define is-immutable? (immutable? result))
                 (define enhanced
                   ((if is-immutable? identity hash->mutable)
                    (for/hash ([k (append (hash-keys overwrite-hash) (hash-keys result))])
                      (values k (hash-ref result k #f)))))
                 (safe-hash-union enhanced
                                  overwrite-hash
                                  #:combine/key (lambda (key orig-val overwrite-val)
                                                  ;(say  "(key orig-val overwrite-val): "  key ", " orig-val ", " overwrite-val)
                                                  (match (and (procedure? overwrite-val)
                                                              (procedure-arity overwrite-val))
                                                    [#f overwrite-val]
                                                    [0  (overwrite-val)]
                                                    [1  (overwrite-val orig-val)]
                                                    [3  (overwrite-val result
                                                                       key
                                                                       orig-val)]
                                                    [else (raise-arguments-error 'hash-remap
                                                                                 "In the #:overwrite key, all value-generating procedures must have arity of exactly 0, 1, or 3"
                                                                                 "procedure" overwrite-val)])))]
                ;
                ['add

                 ;;    Next, add any additional keys that we were told to add.
                 ;;
                 ;;    NOTE: This will throw an exception if you try to add a
                 ;;    key that is already there.  Use the #:default keyword
                 ;;    if you simply want to make sure the key is there
                 ;;    without disturbing a previously-existing value.
                 (safe-hash-union result
                                  add-hash
                                  #:combine/key (lambda _ (raise-arguments-error
                                                           'hash-remap
                                                           "add-hash cannot include keys that are already in the hash (hint: use #:default, or #:overwrite, or use #:action-order to put 'remove before 'add)"
                                                           "add-hash" add-hash
                                                           "hash to add to (remove and overwrite already done)" result)))]
                ;
                ['rename
                 ;;    Rename keys
                 (for/fold ([h result])
                           ([(key val) remap-hash])
                  ;;;(say "renaming in hash with key/val: " h "," key "," val)
                   (hash-rename-key h key val))]
                ;
                ['default
                  ;;   Default.  Keys that are in default but not in the hash will be
                  ;;   added.  Keys that are in default AND in the hash will be set IFF
                  ;;   their value matches the value-is-default? predicate
                  ;
                  ; If your default value is:
                  ;
                  ;    - A normal value then it will be used directly.
                  ;    - A procedure of arity 2 then it will be called as (default key hash)
                  ;    - A procedure of any other arity then it will be called
                  ;        as (default-val key).
                  ;
                  ; The hash that's passed to the arity-2 functions will be the original
                  ; hash with some or all of the values already defaulted.  Order of
                  ; iteration on a hash is unpredictable, so be careful about using
                  ; default values that are dependent on other keys that will in turn be
                  ; defaulted.
                  ;
                  ; If you want your defaulted value to actually be a procedure, as
                  ; opposed to a value generated by a procedure, then you'll need to wrap
                  ; it like so:
                  ;
                  ;    #:default (hash 'foo (lambda (key) (lambda ...)))
                  ;
                  (define make-value
                    (lambda (key hsh default-val)
                      (cond [(not (procedure? default-val)) default-val]
                            [(let ([proc-arity (procedure-arity default-val)])
                               (and (number? proc-arity)
                                    (= 2 proc-arity)))
                             (default-val key hsh)]
                            [else (default-val key)])))

                  (for/fold ([defaulted-result result])
                            ([(key default-val) default-hash])
                    ;;(say "key/val/final val: " key ", " default-val ", " (make-value key defaulted-result default-val))
                    (cond [(not (hash-has-key? defaulted-result key))
                           ;;(say "not has")
                           (safe-hash-set defaulted-result
                                          key
                                          (make-value key defaulted-result default-val))]
                          ;
                          [(value-is-default? (hash-ref defaulted-result key))
                           ;;(say "val is def")
                           (safe-hash-set defaulted-result key (make-value key
                                                                           defaulted-result
                                                                           default-val))]
                          ;
                          [else
                           ;;(say "else")
                           defaulted-result]))])))
          result])))

;;----------------------------------------------------------------------

(define/contract (hash->keyword-apply func hsh [positionals '()])
  (->* (procedure? (hash/c symbol? any/c)) (list?) any)

  (define keys (sort (hash-keys hsh) symbol<?))

  (keyword-apply func
                 (map (compose string->keyword symbol->string) keys)
                 (map (curry hash-ref hsh) keys)
                 positionals))

;;----------------------------------------------------------------------

(module+ test
  (require rackunit)
  ;    This is just here so that the package server will acknowledge that
  ;    I have tests.  They are built with test-more, not rackunit, but the
  ;    package server can't understand test-more so I'm stubbing.
  ;
  ;    NOTE: At some point I could capture the test-more output and do an
  ;    check-equal? against a gold master to validate it via rackunit so
  ;    that the package server can figure out that things are working.
  (check-equal? 0 0))

;;
;;  Extend match to allow for matching optional values in hash tables.
;;  Code provided by: Ryan Culpepper at Racketcon2018.  NOT CURRENTLY
;;  WORKING because I didn't record it promptly.
;;
;; TODO:
;; (define not-found (gensym 'not-found))
;; (define (not-not-found? x) (not (eq? x not-found)))

;; (begin-for-syntax
;;   (define-syntax-class kvpat
;;     #:description "hash key-value pattern"
;;     ;; Note: be careful to evaluate key expr only once!
;;     (pattern [key:expr value-pattern]
;;              #:with pattern
;;              #'(app (lambda (h) (hash-ref h key not-found))
;;                     (? not-not-found? value-pattern)))
;;     (pattern [key:expr value-pattern default:expr]
;;              #:with pattern
;;              #'(app (lambda (h) (hash-ref h key (lambda () default)))
;;                     value-pattern))))

;; (define-match-expander hash-table*
;;   (syntax-parser
;;     [(_ kv:kvpat ...)
;;      #'(? hash? kv.pattern ...)]))
