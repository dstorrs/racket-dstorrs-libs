#lang scribble/manual

@(require (for-label racket)
racket/match
racket/sandbox
scribble/example)

@title{handy/list-utils}
@author{David K. Storrs}

@defmodule[handy/list-utils]

@section{Introduction}

@racketmodname[handy] is a collection of utility libraries aimed at different purposes.  @racketmodname[handy/list-utils] contains (unsurprisingly) list-related utilities.

@(define eval
(call-with-trusted-sandbox-configuration
(lambda ()
(parameterize ([sandbox-output 'string]
[sandbox-error-output 'string]
[sandbox-memory-limit 50])
(make-evaluator 'racket)))))


@(define-syntax-rule (hlu-eval label body ...)
(examples #:label label #:eval ((make-eval-factory '("../list-utils.rkt" racket)))
body ...))

@subsection{Parameters and Related Functions}

The following parameters modify how @racket[vector->dict] and @racket[list->dict] work. Those two functions take a list of keys and a (list/vector) of values and turn them into a @racket[dict?], possibly doing various transformations along the way.

@defparam[current-transform-data-function func (-> any/c any/c pair?)]{This function will be mapped across the keys- and values lists in order to generate the list of pairs that will be passed to the @tech{dict-maker} function stored in the @racket[current-dict-maker-function] parameter. (see below)

Default: @racket[cons]}




@defparam[current-dict-maker-function func (-> (listof pair?) dict?)]{Controls how dicts are created.

Default: @racket[make-hash]}

@defparam[current-transform-dict-function func (-> dict? dict?)]{Takes the dict generated by the @racket[dict-maker] function and returns a (perhaps modified) version.

Default: @racket[identity]}

@defproc[(make-transform-data-func [predicate predicate/c] [replacer (-> any/c any/c)] ... ...) (-> any/c any/c pair?)]{Generates a function suitable for use in @racket[current-transform-data-function].

The resulting function expects to be given two values: a key and its value. The value will be checked to see if it matches any of the predicates supplied to @racket[make-transform-data-func].  If so, the value will be passed to the corresponding @racket[replacer] function.  The final result will be a pair consisting of the key and the final value.

NOTE:  This function is intended for use when there are discrete classes of possible inputs.  The order in which predicates are tested is not guaranteed and it will stop after finding its first match.

@(hlu-eval #f
           (define transformer
             (make-transform-data-func (and/c number? exact?) exact->inexact
                                       string? string-trim
                                       (vectorof string? #:flat? #t) (compose (curry map (compose1 string-titlecase string-trim))
                                                                              vector->list)))
(transformer 'name "       Bob")
(transformer 'pies-eaten 2/3)
(transformer 'friends (vector "mike" "fi" "jesse" "sam"))
(transformer 'unchanged-1 (vector 'mike "fi" "jesse" "sam"))
(transformer 'unchanged-2 (list "mike" "fi" "jesse" "sam"))
)
}

@defproc[(make-transform-data-func* [predicate predicate/c] [replacer (-> any/c any/c)] ... ...) (-> any/c any/c pair?)]{Similar to @racket[make-transform-data-func], this generate a function suitable for use in @racket[current-transform-data-function].

It is guaranteed that the specified predicates will all be tested, that they will be tested in the order provided, and replacements can be chained as the modified value will be carried from one step to the next.

@(hlu-eval #f
           (define transformer
             (make-transform-data-func* (and/c number? exact?) exact->inexact
                                        string? string-trim
                                        (vectorof string? #:flat? #t) vector->list
                                        (listof string?) (curry map (compose1 string-titlecase
                                                                              string-trim)))

             )
(transformer 'name "       Bob")
(transformer 'pies-eaten 2/3)
(transformer 'unchanged (vector 'mike "fi" "jesse" "sam"))
(transformer 'friends-1 (vector "mike" "fi" "jesse" "sam"))
(transformer 'friends-2 (list "mike" "fi" "jesse" "sam"))
)
}

@subsection{Functions}

@defproc[(atom? [v any/c]) boolean?]{
Returns @racket[#f] if v is a pair, @racket[#f] otherwise.}

@defproc[(autobox [v any/c]) list?]{
If v is a list, returns v.  Otherwise, returns (list v)}

@defproc[(compose-fifo [v any/c] ...) procedure?]{
Like @racket[compose] but composes from left to right instead of right to left, so the first listed function is the first to receive the argument(s).}

@defproc[(disjunction [x hash?] [y hash?]) dict-disjunction?]{Takes a pair of hashes and returns a @racket[dict-disjunction] struct that contains a breakdown of the elements; see example below for what the struct looks like.  (NB:  I should either have named the struct @racket[hash-disjunction] or the contract should have been for @racket[dict?]@racketidfont{s} instead of @racket[hash?]@racketidfont{s}.)
@(hlu-eval
#f
(struct dict-disjunction (different only-in-first only-in-second dict-first dict-second) #:transparent)
(pretty-print	(disjunction (hash 'a 1 'b 2 'c 3) (hash 'a 2 'd 4))))}

@defproc[(first* [l list?]) (not/c pair?)]{Returns the first atom in a (possibly nested) list.
@(hlu-eval #f
(first* '(a b c))
(first* '(((a b)))))}

@defproc*[([(in-range-inc [end real?]) stream?]
[(in-range-inc [start real?] [end real?] [step real? 1]) stream?])]{Like @racket[in-range] but includes the endpoint.
@(hlu-eval #f
(for/list ([i (in-range 0 3)])  i)
(for/list ([i (in-range-inc 0 3)])  i)
(for/list ([i (in-range-inc 3)])  i)
(for/list ([i (in-range-inc 0 3 0.5)]) i))}

@defproc[(insert-at [orig list?] [new list?] [idx natural-number/c]) list?]{Add a list at a specified 0-based index of another list
@(hlu-eval #f
(insert-at '(a b c e) '(d) 3))}
@defproc[(list-remf* [#:pred p (-> any/c boolean?) void?]) list?]{Create a list and filter out all elements that match the predicate.

@(hlu-eval #f
(define (get-employees-1 [include-offsite #f])
  (list  'alice (when include-offsite 'bob)))

(define (get-employees-2 [include-offsite #f])
  (list-remf*  'alice (when include-offsite 'bob)))

(get-employees-1)
(get-employees-2)

(define (get-people [exclude none/c]) ; by default, exclude nothing
  (list-remf* #:pred exclude
              (hash 'name 'alice   'employed #t)
              (hash 'name 'bob     'employed #t 'handed 'left)
              (hash 'name 'charlie 'employed #f)
              (hash 'name 'denise  'employed #f)))

(get-people)
(define unemployed (get-people (λ (h) (hash-ref h 'employed))))
unemployed
(define handedness-unknown (get-people (λ (h) (hash-has-key? h 'handed))))
handedness-unknown
)}

@defproc[(list->assoc-list [lst (and/c list? (λ (l) (even? (length l))))]) (listof pair?)]{Converts an even-length list into an associative list.  Pass the result to @racket[make-hash] or @racket[make-immutable-hash] and you have the inverse of hash->list.

@(hlu-eval #f
(list->assoc-list '(a b c d))
)}


@defproc[(list/not-null? [v any/c]) boolean?]{Returns true if @racketidfont{v} is a pair and not @racket['()]. NB: checks for pair, not list, so it treats @racket['(x . y)] as a list.}

@defproc[(list->values [l list?]) any]{Multiple-value return of all items in a list
@(hlu-eval #f
(list->values '(a b c)))}


@defproc[(find-contiguous-runs [data list?]
                               [#:key extract-key (-> any/c any/c) identity]
			       [#:op  op  (-> any/c any/c boolean?) #f])
			       list?]{

 Generate a list of lists where each sublist is a sequence of
 consecutive elements.  For example, if given this list:

    @racket['(1 2 3 5 7 200 201 202 203)]

 Then you would get this result:

    @racket['((1 2 3) (5) (7) (200 201 202 203))]

 Keyword arguments are:

    @racket[#:key] Function to use in order to generate the values that
    define what a run is.  By default this is @racket[identity].  You can
    pass a function of @racket[(-> any/c any/c)] to use instead.

    @racket[#:op] The function to use for determining if two elements are
    contiguous.  By default this is:

      @racket[(lambda (a b) (= (add1 (extract-key a) (extract-key b))))]

   If that doesn't work for your use case, you can pass a function of  @racket[(-> any/c any/c boolean?)]

@(hlu-eval
     #f
     (list->values (find-contiguous-runs (list (hash 'age 17) (hash 'age 18) (hash 'age 27))
     		                         #:key (curryr hash-ref 'age))))
}

@defproc[(remove-duplicates/rec [lst list?] [same? (-> any/c any/c any/c) equal?] [#:key extract-key (-> any/c any/c) identity]) list?]{Recursively remove duplicates in a (possibly nested) list
@(hlu-eval #f

(remove-duplicates/rec '())
(remove-duplicates/rec '(a b c))
(remove-duplicates/rec '(a a b b c))
(remove-duplicates/rec '(a a (a b c) b c))
(remove-duplicates/rec '(x ((x)) y z y))

"; The following will de-dupe its args because (x) equal? (x)"
(remove-duplicates/rec '(a a (a b c) b c) equal? #:key list)
(remove-duplicates/rec '(a a b b c)       equal? #:key list)
(remove-duplicates/rec '(a b c)           equal? #:key list)
(remove-duplicates/rec '()                equal? #:key list)
(remove-duplicates/rec '(x ((x)) y z y)   equal? #:key list)

"; The following will NOT de-dupe its args because (x) NOT eq? (x)"
(remove-duplicates/rec '(a a (a b c) b c) eq? #:key list)
(remove-duplicates/rec '(a a b b c)       eq? #:key list)
(remove-duplicates/rec '(a b c)           eq? #:key list)
(remove-duplicates/rec '()                eq? #:key list)
(remove-duplicates/rec '(x ((x)) y z y)   eq? #:key list)
)
}

@defproc[(remove-nulls [lst list?]) list?]{Filter @racket['()]s out of a list}

@defproc*[([(safe-first [lst list?][default any/c '()]) any/c]
           [(safe-rest  [lst list?][default any/c '()]) any/c])]{
  When @racketidfont{lst} is non-null these work like @racket[first] and @racket[rest] respectively.

When @racketidfont{lst} is null then these return @racketidfont{default}

@(hlu-eval #f
(safe-first '(a b c))
(safe-first '())
(safe-first '() #f)
(safe-rest '(a b c))
(safe-rest '())
(safe-rest '() #f)
)
}.

@defproc[(slice [lst list?][start natural-number/c] [end natural-number/c +inf.0]) list?]{Returns indices @racket[start] up to and including @racket[end].
@(hlu-eval #f
(slice '(a b c d e f g) 2)
(slice '(a b c d e f g) 2 4))}

@defproc*[([(sort-num [lst list?] [#:key key (-> any/c any/c) identity][#:cache-keys? cache? boolean? #f][#:asc? asc? boolean? #t]) list?]
	   [(sort-str [lst list?] [#:key key (-> any/c any/c) identity][#:cache-keys? cache? boolean? #f][#:asc? asc? boolean? #t]) list?]
	   [(sort-sym [lst list?] [#:key key (-> any/c any/c) identity][#:cache-keys? cache? boolean? #f][#:asc? asc? boolean? #t]) list?]
	   [(sort-bool [lst list?] [#:key key (-> any/c any/c) identity][#:cache-keys? cache? boolean? #f][#:asc? asc? boolean? #t]) list?]
	   [(sort-smart [lst list?] [#:key key (-> any/c any/c) identity][#:cache-keys? cache? boolean? #f][#:asc? asc? boolean? #t]) list?])]{Short names for sorting various datatypes.  @racket[sort-smart] will check the type of the first element of the provided list and then trampoline to the appropriate specific sort function.  The @racket[asc?] argument specifies whether the list should be sorted in ascending order (the default) or descending order.}

@defproc[(step-by-n [func (unconstrained-domain-> any/c)][data sequence?] [step-size? exact-positive-integer? 2] [#:return-results? return-results? #t] [#:pass-args-as-list? pass-args-as-list? #f]) (or/c void? list?)]{Step through a sequence @racket[step-size?] elements at a time (or however many are left), passing them to @racket[func].  Normally they will be passed positionally, but if @racket[pass-args-as-list?] is @racket[#t] then @racket[func] will receive a list containing the arguments.  By default the results of @racket[func] will be returned in a list, but if @racket[return-results?] is @racket[#f] then @racket[step-by-n] will return @racket[(void)].

@(hlu-eval
	#f
	(step-by-n list '(a b c d))
	(step-by-n list '(a b c d e))
	(step-by-n hash '(a 1 b 2 c 3 d 4) 4)
	(struct person (name age sex) #:transparent)
	(step-by-n person '(bob 18 male diane 19 female) 3)
	(step-by-n (curry apply person) '(bob 18 male diane 19 female) 3 #:pass-args-as-list? #t)
	(step-by-n displayln (list "foo" "bar" "baz") 1)
	(step-by-n displayln (list "foo" "bar" "baz") 1 #:return-results? #f)
	(define (query-row conn sql vals)
		  "Mock function that pretends to get a row from the database. See (require db) for the actual query-row function."
		  (apply vector vals))
	(step-by-n (curry query-row 'conn "select * from users where first_name = $1 and last_name = $2") '(bob geitz isaac asimov) #:pass-args-as-list? #t))}

@defproc[(get [s any/c] [keys any/c] [def any/c unsupplied-value]) any/c]{
Take a data structure built of nested (hashes, lists, vectors, structs) and retrieve items from it.  Hashes are accessed by key, vectors and lists by index, and structs by function. If the data is not a recognized thing then @racket[get] returns the data.

As a special case, if there is only one key then you can pass it directly instead of in a list.

The optional def argument allows you to specify a default.  The default is returned iff one of the following exceptions is thrown:

        @racket[exn:fail:contract] with message:  "list-ref: index too large for list"
        @racket[exn:fail:contract] with message:  "hash-ref: no value found for key"
        @racket[exn:fail:contract] with message:  "vector-ref: index is out of range"

Note that you can pass functions as 'keys' even when the thing you'll be accessing is not a struct.

@(hlu-eval #f
   (define l '(foo "bar" ("baz" (quux))))
   (define h (make-hash
              `(("foo" . "bar")
                (baz . 7)
                (quux . (foo bar))
                (blag . ,(make-hash '(["baz" . "jaz"]))))))


   (get l 0)
   (get l '(0))
   (get l '(1))
   (get l '(2))
   (get l '(2 0))
   (get l '(188) -11)

   (get h 'quux)
   (get h '(quux 0))
   (get h '(jaz) "not found")

(struct fruit (taste data))
(define apple (fruit 'sweet (hash 'seeds 7)))
(get (hash 'x (list 'a 'b apple 'c))
     (list 'x 2 fruit-data 'seeds))
	)
	}



@section{DEPRECATED}

The following will be removed in a future version.  They were mostly written when I was first learning Racket and didn't realize that there was already something that did the thing I wanted.  Either that, or they simply proved to be unnecessary/not useful.

@defproc[(alist->hash [lst (listof pair?)]) hash?]{An alternate name  for @racket[make-immutable-hash].}

@defproc[(L [v any/c] ...) list?]{An alternate name for @racket[list].}


@defproc[(member-rec [m any/c] [lst any/c]) list?]{Search through a list recursively for all instances of an item, includes ones that are nested inside other instances.  The item can be either a value or a predicate.  Returns a list of all instances; nested items will appear both in their parent and on their own.  e.g.:
@(hlu-eval #f
 (define l '(1 2 (table 1) ((4) 5 (((table 2 (table 3)))))))
 (member-rec 2 l)                  
 (member-rec (curry equal? 2) l)   
 (member-rec number? l)            
 (member-rec (lambda (x) (and (list? x)
 			      (not (null? x))
 			      (equal? (car x) 'table)))
             l)  
)}
