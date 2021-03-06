#lang scribble/manual

@(require (for-label racket handy)
racket/sandbox
scribble/example)

@title{handy/list-utils}
@author{David K. Storrs}

@defmodule[handy]

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
#reader scribble/comment-reader
(examples #:label label #:eval ((make-eval-factory '("../list-utils.rkt" racket)))
body ...))

@subsection{Parameters and Related Functions}

The following parameters modify how @racket[vector->dict] and @racket[list->dict] work. Those two functions take a list of keys and a (list/vector) of values and turn them into a @racket[dict?], possibly doing various transformations along the way.

@defparam[current-dict-maker-function func (-> (listof pair?) any/c)]{Controls how dicts are created.

NOTE:  Despite the name, the contract does not actually require that the output be a @racket[dict?].

Default: @racket[make-hash]}

@defparam[current-transform-data-function func (-> any/c any/c pair?)]{This function will be mapped across the keys- and values lists in order to generate the list of pairs that will be passed to the @tech{dict-maker} function stored in the @racket[current-dict-maker-function] parameter. (see below)

Default: @racket[cons]}

@defparam[current-transform-dict-function func (-> any/c any)]{Takes the dict generated by the @racket[dict-maker] function and returns a (perhaps modified) version.

NOTE:  Despite the name, the contract does not actually require that the output be a @racket[dict?]. One common example of spitting out something other than a @racket[dict?] would be to turn the @racket[dict?] into a @racket[struct].  See the @racketmod[handy/struct] function @racket[hash->struct/kw].

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

@defproc[(first* [l list?]) (not/c pair?)]{Returns the first atom in a (possibly nested) list.
	 @(hlu-eval #f
		    (first* '(a b c))
		    (first* '(((a b)))))}


@defproc[(flatten/convert [func procedure?][lst list?]) list?]{Maps the specified function across the list and flattens the result.  Useful when working with DBs and you need to process the results.}

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

@defproc[(list->assoc-list [lst (and/c list? (λ (l) (even? (length l))))]) (listof pair?)]{Converts an even-length list into an associative list.  Pass the result to @racket[make-hash] or @racket[make-immutable-hash] and you have the inverse of hash->list.

	  @(hlu-eval #f
		     (list->assoc-list '(a b c d))
		     )}

@defproc[(list->dict [raw-keys list?] [data list?]
	     [#:make-keys      key-maker (or/c #f (-> any/c any/c)) #f]
	     [#:transform-data transform-data (-> any/c any/c pair?) (current-transform-data-function)]
	     [#:dict-maker     dict-maker (-> (listof pair?) any/c) (current-dict-maker-function)]
	     [#:transform-dict transform-dict (-> any/c any) (current-transform-dict-function)])
 any]{Takes a list of keys and a list of data, as well as some optional keyword params.  Default functionality is to cons the keys to their respective data then send that to make-hash in order to generate a mutable hash.  Through application of the various optional params you can make it generate anything.

 NOTE: The contract does not actually require that the return value be a dict; if you know what you're doing and want to generate something else, that's fine.

 A dict is either:
 @itemlist[
	   @item{a hash}
	   @item{a vector (uses exact integers as keys)}
	   @item{a list of pairs  (could be either cons pairs or proper non-null lists of any length)}
	   @item{structures that implement the gen:dict interface}
	   ]

 Default values assuming the various parameters listed in the signature have not had their values changed:

 @itemlist[
	   @item{@racket[#:transform-data]     Default: @racket[cons]       Accepts a key and a value, returns a pair.}
	   @item{@racket[#:dict-maker]         Default: @racket[make-hash]  Gets the list of pairs from transform-data.  NOTE:  This generates a mutable hash. That was a bad design choice (I should have used @racket[make-immutable-hash]) but I'm leaving it as-is in order to maintain backwards compatibility.}
	   @item{@racket[#:transform-dict]     Default: @racket[identity]   Operate on the result of dict-maker}
	   @item{@racket[#:make-keys]          Default: @racket[#f]         If set, generates the keys based on the data}
	   ]

 @(hlu-eval #f
	    (list->dict '(foo bar) '(7 8))
	    (list->dict '()        '(65 66) #:make-keys integer->char)
	    (list->dict '(foo bar) '(65 66) #:make-keys integer->char)
	    (list->dict '(foo bar) '(7 8) #:transform-data (lambda (k v) (cons k (add1 v))))
	    (list->dict '(foo bar) '(7 8) #:dict-maker make-immutable-hash)

	    (define (get-metadata-for-person h)
	      "This could get data from anywhere -- DB, REST call, etc"
	      (hash 'last-login 1587761196761 'access-level 'admin))

	    (struct person (name age profile) #:transparent)
	    (define (person/kw #:name name #:age age #:profile profile) (person name age profile))
	    (define (hash->struct/kw struct-ctor h)
	      "See handy/struct for a better version of this function"
	      ; See handy/struct for a better version of this function
	      (define sorted-keys (sort (hash-keys h) symbol<?))
	      (keyword-apply struct-ctor
			     (map (compose string->keyword symbol->string) (sort sorted-keys symbol<?))
			     (map (curry hash-ref h) sorted-keys)
			     '()))
	    (parameterize ([current-dict-maker-function make-immutable-hash])
			  (list->dict '(name age) '(bob 22)
				      #:transform-dict (λ (h)
							  (hash->struct/kw
							   person/kw
							   (hash-set h 'profile (get-metadata-for-person h)))))))}

@defproc[(list->values [l list?]) any]{Multiple-value return of all items in a list
		@(hlu-eval #f
			   (list->values '(a b c)))}

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

@defproc[(list/not-null? [v any/c]) boolean?]{Returns true if @racketidfont{v} is a pair and not @racket['()]. NB: checks for pair, not list, so it treats @racket['(x . y)] as a list.}

@defproc[(multi-partition [#:partitions num-dests exact-positive-integer?]
		  [#:source source list?]
		  [#:filter                 chooser (-> any/c (or/c #f void? exact-nonnegative-integer?)) (let-values ([(more? next) (sequence-generate (in-naturals))])
														      (lambda (x) (modulo (next) num-dests)))]
		  [#:post-process-element   post-process-element   (-> exact-nonnegative-integer? any/c any/c) (lambda (idx elem) elem)]
		  [#:post-process-partition post-process-partition (-> list? any/c) identity]
		  [#:post-process-all-data  post-process-all-data (-> vector? any)  vector->list]
		  )
 any]{This is like group-by, but with some additional properties and capabilities.  Specifically:

 @itemlist[
	   @item{Items will come back in the order you want them to, not in the order they happen to occur in the source.  This is useful if you have different kinds of elements in a list -- for example, given a function that might return a valid result or throw one of various kinds of exceptions, you can multi-partition the results such that all the valid ones are in partition 1 and each of the various kinds of exceptions are all in expected slots.}
	   @item{You can separately post process each element and/or partition, and all of the resulting data.}
	   ]
 @(hlu-eval #f

	    (multi-partition #:partitions 3 #:source '(1 2 3 4 5 6 7) #:filter (curryr modulo 3))
	    (multi-partition #:partitions 3 #:source '(1 2 3 4 5 6 7) #:filter (curryr modulo 3)
			     #:post-process-element (lambda (idx elem) (add1 elem))
			     #:post-process-partition (lambda (lst) (map (curry * 2) lst)))

	    (define lst  '(a b c))
	    (multi-partition #:partitions 1
			     #:filter (lambda (x) (raise "should not get here"))
			     #:source lst)
	    (multi-partition #:partitions 2
			     #:filter (lambda (n) 1)
			     #:post-process-all-data vector->values
			     #:source '())
	    (multi-partition #:partitions 3
			     #:filter (lambda (n) 1)
			     #:post-process-all-data vector->values
			     #:source '())

	    (multi-partition #:partitions 3
			     #:filter (lambda (n) (cond [(zero? (floor n)) 0]
							[(even? (floor n)) 1]
							[(odd?  (floor n)) 2]))
			     #:post-process-all-data vector->values
			     #:source '(1 7 8 0 15.8 -2))

	    (with-handlers ([exn:fail? (λ (e)
					  (displayln "If your match function returns something other than #f or a 0+ natural number then multi-partition throws"))]) 
			   (multi-partition #:partitions 2
					    #:filter (lambda (n) #t)
					    #:post-process-all-data vector->values
					    #:source '(1 7 8 0 15.8 -2 a)))
	    (with-handlers ([exn:fail? (λ (e)
					  (displayln "Returned 8.2 : If your match function returns something other than #f or a 0+ natural number then multi-partition throws"))])
			   (multi-partition #:partitions 2
					    #:filter (lambda (n) 8.2)
					    #:post-process-all-data vector->values
					    #:source '(1 7 8 0 15.8 -2 a)))

	    (multi-partition #:partitions 2
			     #:source '(1 2 3 4 1)
			     #:post-process-partition unique
			     #:post-process-all-data vector->values
			     #:filter (lambda (i) (if (odd? i) 0 1)))
   
	    (multi-partition #:partitions 2
			     #:source '(1 2 3 4 1)
			     #:post-process-partition unique
			     #:post-process-all-data vector->values
			     #:filter (lambda (i)
					(cond [(odd? i) 0]
					      [(= 4 i) #f]
					      [else     1]))
			     )
     
	    (multi-partition #:partitions 2
			     #:source '(1 2 3 4 1)
			     #:post-process-partition unique
			     #:post-process-all-data vector->values
			     #:filter (lambda (i)
					(cond [(odd? i) 0]
					      [(= 8 i)    1])))

	    (multi-partition #:partitions 2
			     #:source '(1 2 3 4 1)
			     #:post-process-element (lambda (x y) (add1 y))
			     #:post-process-all-data vector->values
			     #:filter (lambda (i)
					(cond [(odd? i) 0]
					      [(= 8 i)    1])))
	    (multi-partition #:partitions 3
			     #:source '(a b c d e f g)
			     #:post-process-all-data vector->values)

	    (multi-partition #:partitions 3 #:source '(a b c d e f g))
	    )
 }

@defproc[(pick [lst (non-empty-listof any/c)]) any/c]{Return a random item from @racket[lst].}

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
							}

@defproc[(slice [lst list?][start natural-number/c] [end natural-number/c +inf.0]) list?]{Returns indices @racket[start] up to and including @racket[end].
	 @(hlu-eval #f
		    (slice '(a b c d e f g) 2)
		    (slice '(a b c d e f g) 2 4))}

@defproc*[([(sort-bool [lst list?] [#:key key (-> any/c any/c) identity][#:cache-keys? cache? boolean? #f][#:asc? asc? boolean? #t]) list?]
  [(sort-num [lst list?] [#:key key (-> any/c any/c) identity][#:cache-keys? cache? boolean? #f][#:asc? asc? boolean? #t]) list?]
  [(sort-smart [lst list?] [#:key key (-> any/c any/c) identity][#:cache-keys? cache? boolean? #f][#:asc? asc? boolean? #t]) list?]
  [(sort-str [lst list?] [#:key key (-> any/c any/c) identity][#:cache-keys? cache? boolean? #f][#:asc? asc? boolean? #t]) list?]
  [(sort-sym [lst list?] [#:key key (-> any/c any/c) identity][#:cache-keys? cache? boolean? #f][#:asc? asc? boolean? #t]) list?]
)]{Short names for sorting various datatypes.  @racket[sort-smart] will check the type of the first element of the provided list and then trampoline to the appropriate specific sort function.  The @racket[asc?] argument specifies whether the list should be sorted in ascending order (the default) or descending order.}

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

@defproc[(symbols->keywords [lst (listof symbol?)]) (listof keyword?) ]{Take a list of symbols, sort them, convert them into keywords.  Useful in combination with @racket[keyword-apply].}

@defproc[(unique [lst list?]  [same? (-> any/c any/c boolean?) equal?] [#:key key-maker (-> any/c any/c) identity]) list?]{Convenient alias for running @racket[remove-nulls] on the result of @racket[remove-duplicates].  @racket[same?] determines if two elements are the same and therefore the latter should be removed.  @racket[key-maker] determines the actual value to test.}

@defproc[(unwrap-list [lst list?]) list?]{If @racket[lst] is a 1-element list, return the @racket[car] of @racket[lst].  Otherwise, return @racket[lst].

    @(hlu-eval #f
	       (unwrap-list '(a b c))
	       (unwrap-list '((a b c) (d e f)))
	       (unwrap-list  '((a b c))))}

@defproc[(vector->dict [raw-keys         list?]
	               [data             vector?]
                       [#:make-keys      key-maker (or/c #f (-> any/c any/c)) #f]
                       [#:transform-data transform-data (-> any/c any/c pair?) (current-transform-data-function)]
                       [#:dict-maker     dict-maker (-> (listof pair?) any/c) (current-dict-maker-function)]
                       [#:transform-dict transform-dict (-> any/c any) (current-transform-dict-function)]
	       )
	       any]{
 This is a trampoline to @racket[list->dict].  It simply calls @racket[vector->list] on the data and then passes that and all the other arguments to @racket[list->dict].
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

@secref{Introduction}
