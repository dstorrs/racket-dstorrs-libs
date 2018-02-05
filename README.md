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
* struct.rkt       => create structs via keyword and convert hashes to structs
* test-more.rkt    => better (IMO) testing module; largely an expy of Perl's Test::More
* thread.rkt       => convenience functions for running code in a thread
* try.rkt          => syntactic sugar over with-handlers & dynamic-wind
* utils.rkt        => overly bloated collection of random things. Should be split up.
* web.rkt          => retrieve and process web pages and/or local HTML files


Use these by:

    In Racket code: (require "../path/to/libdir/handy/HTML-Element.rkt")

Or, put this in your .bashrc (or other shell setup file):

    export PLTCOLLECTS=/path/to/libdir:$PLTCOLLECTS

Then in your code you can use:

    (require handy/HTML-Element)

For me, this would be:	

    In .bashrc:     export PLTCOLLECTS=/Users/dstorrs/projects/libs/racket/handy:$PLTCOLLECTS
    In Racket code: (require handy/HTML-Element)


# VERSION HISTORY

1.2.7	  - Initial release to Racket package server.  Value chosen completely arbitrarily.