#!/usr/bin/env racket

#lang racket

(require dstorrs/test-more
         racket/runtime-path
         (planet neil/html-parsing:3:0)
         sxml
         "../list-utils.rkt"
         "../HTML-Element.rkt")

(define-runtime-path thisdir ".")
(define test-file (build-path thisdir "some_HTML-Element_test_data.html"))

(println test-file)

(define (hash-key-is? h k v)
  (and (hash-has-key? h k)
       (equal? (hash-ref h k) v)))

;;--------------------------------------------------

(define html-string-newlines
  (string-append
   "Hey, everyone! I'm new here, participating in such quest for the first time. I'm still learning about the system, trying to figure out how it works.<br>"
   "<br>"
   "For now I'll just cast my vote I guess:<br>"
   "<br>"
   "[1] Ninjutsu Specialist, you're throwing fire from behind the meatshields. Your elemental affinity is:<br>"
   "[2] Earth"
   ))

(define an-html-string
  (string-append
   "<html><head><title>My title</title></head><body>"
   "<div class=\"chapter1\">"
   "<p class=\"hi hello aloha \n\" style=\"border: 1px solid red;\">Hello world</p>"
   "</div>"

   "<div class=\"chapter2 \">"  ;; includes 'hi' but isn't of that class
   "<p style=\"border: 1px solid red; \">Bye, world</p>"
   "</div>"

   "<a href=\"http://cnn.com\">CNN</a>"
   "<p class=\"info highway\"><b>Testing <a href=\"http://google.com\">Google</a></b>!</p>"

   "<div class=\"footer\">This is the footer</div>"
   "</body></html>"))

(define an-html
  (html->xexp
   (open-input-string an-html-string)))

(define html-with-newlines
  (html->xexp
   (open-input-string html-string-newlines)))
;;
;; '(*TOP* "Hey, everyone! I'm new here, participating in such quest
;; for the first time. I'm still learning about the system, trying to
;; figure out how it works." (br) (br) "For now I'll just cast my vote
;; I guess:" (br) (br) "[1] Ninjutsu Specialist, you're throwing fire
;; from behind the meatshields. Your elemental affinity is:" (br) "[2]
;; Earth")


;; (*TOP*
;;  (html
;;   (head (title "My title"))
;;   (body
;;    (div (@ (class "chapter1")) (p (@ (class "hi") (style "border: 1px solid red;")) "Hello world"))
;;    (div (@ (class "chapter2")) (p (@ (style "border: 1px solid red; ")) "Bye, world"))
;;    (a (@ (href "http://cnn.com")) "CNN")
;;    (p (b "Testing " (a (@ (href "http://google.com")) "Google")) "!")
;;    (div (@ (class "footer")) "This is the footer")
;;    )))

;;if it's an @list, skip it
;;otherwise, if it's a string, append it to the recursion



;;    The paragraph from an-html that has the 'hi' class
(define hi-paragraph   (third (second (third (second an-html)))))
(define hi-paragraph-attribs (second hi-paragraph))

;;    The paragraph from an-html that contains the google link
(define google-paragraph (fifth (third (second an-html))))


;;======================================================================

(void (ok 1 "verify harness working"))

(when #t
  (test-suite
   "attr-hash: make attribute hashes from elements and attribute lists"
   (ok (sxml:element? hi-paragraph)  "hi-paragraph is an xexp")
   (ok (hash? (attr-hash hi-paragraph)) "gets attribs hash from elem")

   (is (attr-hash "foo") (hash)   "returns empty hash when no attrs found (string source)")
   (is (attr-hash an-html) (hash) "returns empty hash when no attrs found (xexp source)")
   (is (attr-hash hi-paragraph)
       (hash 'class "hi hello aloha" 'style "border: 1px solid red;")
       "got a hash full of attributes from the 'hi-paragraph' data")
   )
  )

(when #t
  (test-suite
   "has-attr?"

   (is-false (has-attr? hi-paragraph 'zort)
             "hi-paragraph correctly does not have attribute 'zort")
   (is (has-attr? hi-paragraph 'class)
       "hi hello aloha"
       "hi-paragraph does have expected class")
   (is (has-attr? hi-paragraph 'class "hello")
       "hi hello aloha"
       "hi-paragraph has expected class and value of that class matches 'hello'")
   )
  )

(when #t
  (test-suite
   "TEST CASE: text-of"
   (is (text-of hi-paragraph)
       "Hello world"
       "(text-of hi-paragraph)")

   (is (text-of google-paragraph)
       "Testing Google !"
       "(text-of google-paragraph)")

   (is (text-of an-html)
       "My title Hello world Bye, world CNN Testing Google ! This is the footer"
       "(text-of an-html)")


   (is (text-of (look-down an-html #:tag 'title))
       "My title"
       "(text-of (look-down an-html #:tag 'title))")

   (is (text-of html-with-newlines)
       "Hey, everyone! I'm new here, participating in such quest for the first time. I'm still learning about the system, trying to figure out how it works. \n \n For now I'll just cast my vote I guess: \n \n [1] Ninjutsu Specialist, you're throwing fire from behind the meatshields. Your elemental affinity is: \n [2] Earth"
       "(text-of html-with-newlines)")

   ))


(when #t
  (test-suite
   "TEST-CASE: look-down"

   ;;    Retrieve the text of all <p> tags
   (is (look-down an-html #:tag 'p text-of)
       '("Hello world" "Bye, world" "Testing Google !")
       "(look-down an-html #:tag 'p text-of)")


   ;;    Retrieve all attrs of the 'p' tag with class "hi"
   (is (look-down an-html
                  #:match (lambda (x)  ;; Check that this is the particular <p> we want
                            (and ((ntype?? 'p) x)
                                 (hash-key-is? (attr-hash x) 'class "hi hello aloha")))
                  (compose list attr-hash))
       (list #hash((style . "border: 1px solid red;") (class . "hi hello aloha")))
       "Retrieved all attrs of the 'p' tag with class \"hi\"")

   ;;    Retrieve all URLs
   (is (look-down an-html #:tag 'a (lambda (el) (list (has-attr? el 'href))))
       '("http://cnn.com" "http://google.com")
       "(look-down an-html #:tag 'a (lambda (el) (list (has-attr? el 'href))))")

   ;;    Retrieve the 'title' text
   (is (look-down an-html #:tag 'title (compose list text-of))
       '("My title")
       "(look-down an-html #:tag 'title (compose list text-of))")


   ;;     Retrieve the text of the <p class="not-me"> tag. (Which doesn't
   ;;     exist.)  Should return '()
   (is (look-down hi-paragraph
                  #:tag 'p
                  #:attr '(class . "not-me")
                  (compose list text-of))
       '()
       "correctly returned '() when asked to find a <p> that wasn't there")

   ;;    Retrieve the text of a 'p' tag that has a class.  Use the
   ;;    #:attr keyword to specify that it must be a <p> with the
   ;;    'class' attr.
   (is (look-down hi-paragraph
                  #:tag 'p
                  #:attr 'class
                  text-of)
       '("Hello world")
       "#:tag and #:attr keywords work together smoothly when attr is just a symbol")

   ;;    Retrieve the text of the <p class="hi"> tag.  Use the #:attr
   ;;    keyword to specify that it must be a <p> with the 'class'
   ;;    attr that has a value of 'hi'.
   (is (look-down an-html
                  #:tag 'p
                  #:attr '(class . "hi")
                  text-of)
       '("Hello world")
       "when #:attr keyword is a dotted pair with string cdr then the value must exactly match the what's in the document being scanned")

   ;;    Retrieve the text of all divs that are of class 'chapter*'.
   ;;    Use the #:attr keyword with a regex to specify that it must be
   ;;    a <div> with the 'class' attr that has a value matching
   ;;    /chapter.*/
   ;;
   (is (look-down an-html
                  #:tag 'div
                  #:attr '(class . #px"^chapter.*")
                  text-of)
       '("Hello world" "Bye, world")
       "#:attr '(class . #px\"^chapter.*\") means 'where the class attribute's value matches this regex")

   ;;    Retrieve all divs.  Let the action default to 'list' instead of
   ;;    passing it explicitly.
   ;;
   (is (map text-of (look-down an-html
                               #:tag 'div
                               #:attr '(class . #px"^chapter.*")))
       '("Hello world" "Bye, world")
       "results of look-down are autoboxed.  Can successfully retrieve more than one element")
   )
  )

(when #t
  (test-suite
   "html-element-from"

   (is (html-element-from an-html-string)
       (html->xexp an-html-string)
       "html-element-from works for strings that are valid HTML")

    (is (html-element-from (path->string test-file))
        (html->xexp (open-input-file test-file))
       "html-element-from works for strings that are relative filepaths")

    (is (html-element-from test-file)
        (html->xexp (open-input-file test-file))
       "html-element-from works for paths")

   )
  )

(done-testing)
