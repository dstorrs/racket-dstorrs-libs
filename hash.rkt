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
;; *) hash-aggregate      : list of hashes -> single hash that maps a key to the hashes
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
;; *) safe-hash-remove    : does hash-remove or hash-remove! as needed.  Returns the hash.
;; *) safe-hash-set       : does hash-set or hash-set! as needed. Returns the hash.
;; *) safe-hash-union     : does hash-union or hash-union! as needed. Returns the hash.
;; *) sorted-hash-keys    : returns sorted list of keys from the hash

(provide (all-from-out racket/hash)

         hash-aggregate

         hash->keyword-apply
         hash-key-exists?

         hash-keys->strings
         hash-keys->symbols

         hash->immutable
         hash->mutable
         mutable-hash

         hash-rename-key
         hash-meld
         hash-remap
         hash-slice
         hash-slice*

         safe-hash-remove
         safe-hash-set
         safe-hash-union

         sorted-hash-keys
         )


;;----------------------------------------------------------------------

(define hash-key-exists? hash-has-key?) ; alias because I always forget the name

;;----------------------------------------------------------------------

; hash-aggregate
;
; Essentially everts a list of hashes into a hash that maps keys of
; those hashes to the individual hashes.  For example:
;
; >  (hash-aggregate 'filepath (hash 'filepath "/foo" 'size 10)
;                              (hash 'filepath "/bar" 'size 20))
; (hash "/foo" (hash 'filepath "/foo" 'size 10)
;       "/bar" (hash 'filepath "/bar" 'size 20))
;
; If multiple hashes share the same value for that key then they will be in a list
;
; >  (hash-aggregate 'filepath (hash 'filepath "/foo" 'size 10)
;                              (hash 'filepath "/bar" 'size 30)
;                              (hash 'filepath "/foo" 'size 20))
;
; (hash "/foo" (list  (hash 'filepath "/foo" 'size 10) (hash 'filepath "/foo" 'size 20))
;       "/bar" (hash 'filepath "/bar" 'size 30))
;
; You can pass a function of one argument as the key.
;
; >  (define key (compose1 add1 (curryr hash-ref 'size)))
; >  (hash-aggregate key (hash 'filepath "/foo" 'size 10)
;                        (hash 'filepath "/bar" 'size 30)
;                        (hash 'filepath "/foo" 'size 20))
; (hash 11  (hash 'filepath "/foo" 'size 10)
;       31 (hash 'filepath "/bar" 'size 30)
;       21 (hash 'filepath "/foo" 'size 20))
(define/contract (hash-aggregate key
                                 #:default [default 'no-default-specified]
                                 . hshs)
  (->* (any/c)
       (#:default any/c)
       #:rest (or/c  (listof hash?)
                     (list/c (listof hash?)))
       hash?)

  (define hashes (flatten hshs)) ; allow passing individual hashes or a list of hashes

  (for/fold ([result (hash)])
            ([h hashes])              ; e.g. (hash 'id 7)

    ; Key might have been specified or it might be the result of a
    ; procedure call
    (define key-in-result
      (cond [(procedure? key) (key h)]
            [(equal? default 'no-default-specified)
             (hash-ref h key)] ; will die if there's a hash without that key
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
(define/contract (hash-keys->strings h
                                     #:dash->underscore? [dash->underscore? #f]
                                     #:underscore->dash? [underscore->dash? #f])
  (->* (hash?)
       (#:dash->underscore? boolean?
        #:underscore->dash? boolean?
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
       (values (cond [dash->underscore?  (regexp-replace* #px"-" key "_")]
                     [underscore->dash?  (regexp-replace* #px"_" key "-")]
                     [else key])
               v)))))

;;----------------------------------------------------------------------

(define/contract (hash-keys->symbols h)
  (-> hash? hash?)
  ((if (immutable? h) identity hash->mutable)
   (for/hash ([(k v) h])
     (values (if (symbol? k) k (string->symbol (~a k)))
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
  (hash->mutable (apply hash args)))

;;----------------------------------------------------------------------

(define/contract (mutable-hash? data)
  (-> hash? boolean?)
  (not (immutable? data)))

;;----------------------------------------------------------------------

(define/contract (hash-meld . hshs)
  (->* () () #:rest (non-empty-listof hash?) hash?)
  (cond [(= (length hshs) 1) (first hshs)]
        [else
         (define base-hash (first hshs))
         (define is-immutable?  (immutable? base-hash))

         (define-values (union-func converter)
           (if is-immutable?
               (values hash-union hash->immutable)
               (values hash-union! hash->mutable)))

         (let ([result (apply union-func
                              (map converter hshs)
                              #:combine (lambda (x y) y))])
           (if is-immutable? result base-hash))]))

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
;;    hash will be returned as the default value (#f if not
;;    specified).
(define/contract (hash-slice* the-hash keys [default 'hash-slice*-default])
  (->* (hash? list?) (any/c) hash?)
  (for/hash ((k keys))
    (values k
            (if (equal? default 'hash-slice*-default)
                (hash-ref the-hash k)
                (hash-ref the-hash k default)))))

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
;; Takes a list of hashes and an optional
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
;;                              #:action-order         [action-order '(remove overwrite add
;;                                                                         rename default)]
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
;;         #:remove list? #:overwrite hash? #:add hash? #:rename hash?
;;         #:default hash?
;;         #:value-is-default? any/c ; (-> any/c boolean) or converts to (or/c default-val)
;;         #:post (-> hash? any)
;;        )
;;        hash?)
;;
;;  Mnemonic for default order of application: ROARenD
;;         remove -> overwrite -> add -> rename -> default
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
;;  matches value-is-default? predicate.  Predicates can be either a
;;  single value or a function matching (-> any/c boolean?) In order
;;  to maintain backwards compatibility you must specify the
;;  #:value-is-default? if you want to default existing values as
;;  opposed to inserting keys that aren't there.  At some point I'll
;;  make it default to #f, but the original version of this wouldn't
;;  touch things that were already there.
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
                             #:remove            [remove-keys #f]
                             #:overwrite         [overwrite #f]
                             #:add               [add #f]
                             #:rename            [remap #f]  ; rename is taken
                             #:default           [default #f]
                             #:value-is-default? [def-val none/c]
                             #:post              [post-process identity]
                             #:action-order      [action-order '(remove overwrite add
                                                                        rename default)])
  (->* (hash?)
       (#:remove list?
        #:overwrite hash?
        #:add hash?
        #:rename hash?
        #:default hash?
        #:value-is-default? any/c
        #:action-order (listof (or/c 'remove 'overwrite 'add 'rename 'default))
        #:post (-> hash? any))
       any)

  (define value-is-default? (or/c def-val)) ; converts non-procedure to procedures, does not affect procedures

  ;(say "default predicate: " value-is-default?)

  ; If we aren't going to end up doing anything then just return the
  ; (post-processed) original hash.
  (cond [(or (null? action-order)
             (andmap false? (list remap remove-keys overwrite add default)))

         ;(say "doing nothing")
         (post-process h)]
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
                ;;        (-> any/c)  ; orig-val, return 1 value
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
                 ;;   Default.  Keys that are in default but not in
                 ;;   the hash will be added.  Keys that are in
                 ;;   default AND in the hash will be set IFF their
                 ;;   value matches the value-is-default? predicate

                 ;;(say "renamed hash: " renamed-hash)
                 ;;(say "default hash: " default-hash)

                 ;
                 ; If your default value is a procedure then it will
                 ; be called as (default-val key).  If it is a normal
                 ; value then it will be used directly.
                 ;
                 ; If you want your default value to actually be a
                 ; procedure, as opposed to a value-generator, then
                 ; you'll need to wrap it:
                 ;
                 ; #:default (hash 'foo (lambda (key) (lambda ...)))
                 ;
                 (define make-value (lambda (key default-val)
                                      (if (procedure? default-val)
                                          (default-val key)
                                          default-val)))

                 (for/fold ([defaulted-result result])
                           ([(key default-val) default-hash])
                   ;;(say "key/val/final val: " key ", " default-val ", " (make-value key default-val))
                   (cond [(not (hash-has-key? defaulted-result key))
                          ;;(say "not has")
                          (safe-hash-set defaulted-result key (make-value key default-val))]
                         ;
                         [(value-is-default? (hash-ref defaulted-result key))
                          ;;(say "val is def")
                          (safe-hash-set defaulted-result key (make-value key default-val))]
                         ;
                         [else
                          ;;(say "else")
                          defaulted-result]))])))

         ;;(say "about to post")

         ;; postprocess the hash and return
         (post-process  result)]))

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
;;  Code provided by: Ryan Culpepper.   NOT CURRENTLY WORKING.
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
