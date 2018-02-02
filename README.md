# NAME  handy

# DESCRIPTION

A set of convenience libraries:

* HTML-Element.rkt => adds searching and access functions missing from html/parsing 
* db.rkt           => database manipulation, exceptions, convenient accessors, etc
* exceptions.rkt   => easy creation and manipulation of exception structs
* fsmonitor.rkt    => filesystem monitoring
* json.rkt         => extends the racket 'json' library with a valid-json? predicate
* list-utils.rkt   => large variety of list-related functionality that's missing from racket
* sql.rkt          => functions for generating SQL
* stats.rkt        => 
* struct.rkt       => 
* test-more.rkt    => 
* thread.rkt       => 
* try.rkt          => 
* utils.rkt        => 
* web.rkt          => 


Use these by:

    In Racket code: (require "../path/to/libdir/handy/HTML-TreeBuilder.rkt")

Or, put this in your .bashrc (or other shell setup file):

    export PLTCOLLECTS=/path/to/libdir:$PLTCOLLECTS

Then in your code you can use:

    (require handy/HTML-TreeBuilder)

For me, this would be:	

    In .bashrc:     export PLTCOLLECTS=/Users/dstorrs/projects/libs/racket/handy:$PLTCOLLECTS
    In Racket code: (require handy/HTML-Element)
