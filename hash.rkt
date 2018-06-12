#lang racket

(require racket/hash) ; for hash-union

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
;; *) safe-hash-remove    : does hash-remove or hash-remove! as needed.  Returns the hash.
;; *) safe-hash-set       : does hash-set or hash-set! as needed. Returns the hash.
;; *) sorted-hash-keys    : returns sorted list of keys from the hash

(provide hash->keyword-apply
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

         safe-hash-remove
         safe-hash-set

         sorted-hash-keys
         )



;;----------------------------------------------------------------------

(define hash-key-exists? hash-has-key?) ; alias because I always forget the name

;;----------------------------------------------------------------------

;;    Convert the keys of a hash from whatever they are to strings.
;;    Optionally, you can convert dashes in the key to underscores or
;;    vice versa; the normal use case is when inserting into a
;;    database and you want keys to match field names.  The normal
;;    string representation of a vector or list isn't terribly useful
;;    (e.g. (vector 'a 'b) => "#(a b)"), so we change them to be the
;;    concatenation of their elements with '-' as a
;;    separator. (e.g. (vector 'a 'b) => "a-b")
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

;;    Takes a list of keys, returns the hash value for each of those keys
(define/contract (hash-slice the-hash keys)
  (-> hash? list? list?)
  (for/list ((k keys))
    (hash-ref the-hash k)))

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
;;                              #:remove    [remove-keys '()]
;;                              #:overwrite [overwrite   #f ]
;;                              #:add       [add         #f ]
;;                              #:rename    [remap       #f ]
;;                              #:default   [default     #f ]
;;                              #:post      [post        identity]
;;                              )
;;   (->* (hash?)
;;        (#:rename hash? #:add hash? #:overwrite hash? #:remove list? #:default hash?
;;         #:post (-> hash? any)
;;        hash?)
;;
;;  Order of application mnemonic:  ROARenD. Remove. Overwrite. Add. Rename. Default.
;;
;;    This will munge hashes any way you like.  You can rename keys,
;;    remove keys, overwrite the value of keys, add new keys, and set
;;    default values.  The order of application is: remove ->
;;    overwrite -> add -> rename -> default.
;;
;;    The return value generally won't be eq? to the input, but it is
;;    guaranteed to be of the same type (mutable / immutable)
;;
;;    NOTE: You could do all this via appropriate application of the
;;    built-in 'hash-union' function, but hash-remap makes it clearer
;;    what you want and will handle mutable or immutable hashes
;;    without complaining.
;;
;; EXAMPLES:
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
;;    signature:
;;
;;        (-> hash? any/c any/c any/c)  ; takes a hash, key, orig-val.  Returns one value
;;
;;    If you actually want to pass in a procedure (e.g. if you're
;;    building a jumptable) then you'll have to wrap it like so:
;;
;;        (lambda (hsh key val orig-val)  ; the 'generate a value' procedure
;;            (lambda ...))               ; the procedure it generates
;;
;;    If you ask to overwrite keys that are not there, they will be added.
;;
;;  ADD additional keys
;;
;;    NOTE: This will throw an exception if you try to add a key that
;;    is already there. If you want to force a key to a value then use
;;    #:overwrite and it will be added or set as necessary.  If you
;;    want to be sure that a hash has a key then use #:default and it
;;    will only be added if it's not there.
;;
;;    (define h (hash 'group 'fruit   'color 'red    'type 'apple))
;;    (hash-remap h #:add (hash 'subtype 'honeycrisp))
;;       => (hash 'group 'fruit 'color 'red 'type 'apple 'subtype 'honeycrisp))
;;
;;    (hash-remap h #:add (hash 'group 'tasty)) => EXCEPTION
;;
;;  RENAME keys
;;
;;    (define h (hash 'group 'fruit   'color 'red    'type 'apple))
;;    (hash-remap h #:rename (hash 'color 'shade  'type 'species )
;;       => (hash 'group 'fruit    'shade 'red    'species 'apple)
;;
;;  DEFAULT values for keys that aren't there but don't touch ones that are
;;
;;  (hash-remap (hash 'x 1)      #:default (hash 'y 2)) => (hash 'x 1 'y 2)
;;  (hash-remap (hash 'x 1 'y 7) #:default (hash 'y 2)) => (hash 'x 1 'y 7)
;;
;;      As with the #:overwrite parameter, you can have your default
;;      values generated if you want, although the procedure only
;;      takes one argument (the key that will be set).  Again, if you
;;      actually want to have the value be a procedure then you'll
;;      need to wrap it.
;;
;;  (hash-remap (hash 'x 1) #:default (hash 'y (lambda (key) (~a key))))
;;     => (hash 'x 1 'y "y")
;;
;; COMPLETE EXAMPLE
;;    (define h (hash 'group 'fruit   'color 'red    'type 'apple))
;;    (hash-remap h  #:remove    '(group)
;;                   #:overwrite (hash 'color 'green   'type 'granny-smith)
;;                   #:add       (hash 'vendor 'bob)
;;                   #:rename    (hash 'vendor 'seller)
;;                   #:default   (hash 'group "group" 'taste ~a))
;;
;;       => (hash 'group  "group"        ; removed via #:remove, then set via #:default
;;                'color  'green         ; overwritten
;;                'type   'granny-smith  ; overwritten
;;                'seller 'bob           ; added
;;                'taste  "taste")       ; defaulted (NB: generated from key name)
;;
;;
(define/contract (hash-remap h
                             #:rename    [remap #f]
                             #:remove    [remove-keys #f]
                             #:overwrite [overwrite #f]
                             #:add       [add #f]
                             #:default   [default (hash)]
                             #:post      [post-process identity]
                             )
  (->* (hash?)
       (#:rename hash? #:add hash? #:overwrite hash? #:remove list? #:default hash?
        #:post (-> hash? any))
       any)

  ; Just return the (post-processed) original hash unless we are going
  ; to rename, remove, overwrite, add, or default something.
  (cond [(and (andmap false? (list remap remove-keys overwrite add))
              (null? (hash-keys default)))
         (post-process h)]
        [else
         ;
         ; Okay, we're going to make some sort of change
         (define h-is-immutable? (immutable? h))

         (define union-func (if h-is-immutable? hash-union hash-union!))

         (define (empty-hash) (if h-is-immutable? (hash) (make-hash)))
         (define overwrite-hash (or overwrite (empty-hash)))
         (define add-hash       (or add       (empty-hash)))
         (define remap-hash     (or remap     (empty-hash)))

         ;; (say "original hash: " h
         ;;      "\n\t immutable?     " (immutable? h)
         ;;      "\n\t overwrite:     " overwrite-hash
         ;;      "\n\t add:           " add-hash
         ;;      "\n\t remap-hash:    " remap-hash
         ;;      "\n\t default-hash:  " default-hash)

         ;;    First, remove any values we were told to remove,
         (define base-hash
           (apply (curry safe-hash-remove h) (or remove-keys '())))

         ;;(say "hash after remove: " base-hash)

         ;;    Now, overwrite any values from the original hash that we
         ;;    were told to overwrite.  If the new value is a procedure
         ;;    then it will be invoked and its result will be the new
         ;;    value.  The procedure must have the signature:
         ;;
         ;;        (-> hash? any/c any/c any/c)  ; hash, key, orig-val, return one value
         ;;
         ;;    The arguments will be: the hash we're updating, the key
         ;;    we're updating, and the original value.  It must return a
         ;;    single value.
         ;;
         ;;    If you actually want to pass in a procedure (e.g. if you're
         ;;    building a jumptable) then you'll have to wrap it like so:
         ;;
         ;;        (lambda (hsh key val orig-val)  ; the 'generate a value' procedure
         ;;            (lambda ...))               ; the procedure it generates
         ;;
         ;;  NB: hash-union! modifies its target in place and then returns
         ;;  #<void>, because of course it does.  As a result, we need to
         ;;  check whether we're dealing with an immutable hash in order to
         ;;  know what to return.
         (define overwritten-hash
           (let ([hsh (union-func base-hash
                                  overwrite-hash
                                  #:combine/key (lambda (key orig-val overwrite-val)
                                                  ;(say "entering combiner with args: " (string-join (map ~v (list key orig-val overwrite-val)) "; "))
                                                  (cond [(procedure? overwrite-val)
                                                         ;(say "proc: " overwrite-val)
                                                         (overwrite-val base-hash
                                                                        key
                                                                        orig-val)]
                                                        [else overwrite-val])))])
             ;(say "finished overwrite")
             (if (void?  hsh) base-hash hsh))) ; void if we're dealing with mutable hash

         ;;    Next, add any additional keys that we were told to add.
         ;;
         ;;    NOTE: This will throw an exception if you try to add a key
         ;;    that is already there.  Use the #:default keyword if you
         ;;    simply want to make sure the key is there.
         (define hash-with-adds
           (let ([hsh (union-func overwritten-hash
                                  add-hash
                                  #:combine/key (lambda _ (raise-arguments-error
                                                           'hash-remap
                                                           "add-hash cannot include keys that are in base-hash"
                                                           "add-hash" add-hash
                                                           "hash to add (remove and overwrite already done)" overwritten-hash)))])
             (if (void? hsh) overwritten-hash hsh))) ; it's void when using mutable hash

         ;;    Rename keys
         (define renamed-hash
           (for/fold ([h hash-with-adds])
                     ([(key val) remap-hash])
             ;(say "renaming in hash with key/val: " h "," key "," val)
             (hash-rename-key h key val)))

         ;;   Set defaults
         (define keys-to-default
           (set-subtract (list->set (hash-keys default))
                         (list->set (hash-keys renamed-hash))))

         (cond [(null? keys-to-default)  (post-process renamed-hash)]
               [else
                (post-process
                 (union-func renamed-hash
                             (for/hash ([key keys-to-default])
                               (values key
                                       (let ([val (hash-ref default key)])
                                         (cond [(procedure? val) (val key)]
                                               [else val]))))))])]))

;;----------------------------------------------------------------------

(define/contract (hash->keyword-apply func hsh [positionals '()])
  (->* (procedure? (hash/c symbol? any/c)) (list?) any)

  (define keys (sort (hash-keys hsh) symbol<?))

  (keyword-apply func
                 (map (compose string->keyword symbol->string) keys)
                 (map (curry hash-ref hsh) keys)
                 positionals))

;;----------------------------------------------------------------------
