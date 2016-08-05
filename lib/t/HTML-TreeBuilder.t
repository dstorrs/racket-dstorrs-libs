#!/usr/bin/env racket

#lang racket

(require rackunit
		 racket/runtime-path
		 (planet neil/html-parsing:3:0)
		 sxml
		 "../list-utils.rkt"
		 "../HTML-TreeBuilder.rkt")

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

(check-equal? 1 1) ;; verify harness working

(test-case
 "TEST CASE: make attribute hashes from elements and attribute lists"
 (check-equal? #t (sxml:element? hi-paragraph) "hi-paragraph is an xexp")
 (check-equal? #t
			   (hash? (attr-hash hi-paragraph))
			   "gets attribs hash from elem")
 (check-equal? (attr-hash "foo") '#hash())
 (check-equal? (attr-hash an-html) '#hash())
 (check-equal? (attr-hash hi-paragraph)
			   '#hash((class . "hi hello aloha") (style . "border: 1px solid red;")))
)

(test-case
 "TEST CASE: has-attr?"

 (check-equal? (has-attr? hi-paragraph 'zort)
			  #f)
 (check-equal? (has-attr? hi-paragraph 'class)
			   "hi hello aloha")
 (check-equal? (has-attr? hi-paragraph 'class "hello")
			   "hi hello aloha")
)


(test-case
 "TEST CASE: text-of"
 (check-equal? (text-of hi-paragraph)
			   "Hello world")
 (check-equal? (text-of google-paragraph)
			  "Testing Google !")
 (check-equal? (text-of an-html)
			   "My title Hello world Bye, world CNN Testing Google ! This is the footer")


  (check-equal? (text-of (look-down an-html
								   #:tag 'title
								   (lambda (x) (list x))))
			   "My title")
  (check-equal? (text-of html-with-newlines)
				"Hey, everyone! I'm new here, participating in such quest for the first time. I'm still learning about the system, trying to figure out how it works. \n \n For now I'll just cast my vote I guess: \n \n [1] Ninjutsu Specialist, you're throwing fire from behind the meatshields. Your elemental affinity is: \n [2] Earth")

  )



(test-case
 "TEST-CASE: look-down"

 ;;    Retrieve the text of all <p> tags
 (check-equal?
  (look-down an-html
			 #:tag 'p
			 (lambda (el) (text-of el)))
  '("Hello world" "Bye, world" "Testing Google !")) 

 
 ;;    Retrieve all attrs of the 'p' tag with class "hi"
 (check-equal?
  (look-down an-html
			 #:match (lambda (x)  ;; Check that this is the particular <p> we want
					   (let ((h (attr-hash x)))
						 (and ((ntype?? 'p) x)
							  (hash-key-is? h 'class "hi hello aloha"))))
 			 (lambda (x) (list (attr-hash x))))
  (list #hash((style . "border: 1px solid red;") (class . "hi hello aloha"))))

 ;;    Retrieve all URLs
 (check-equal?
  (look-down an-html
			 #:tag 'a
			 (lambda (el)
			   (list (has-attr? el 'href))))
  '("http://cnn.com" "http://google.com"))

 ;;    Retrieve the 'title' text
 (check-equal?
  (look-down an-html    
			 #:tag 'title
 			 (lambda (el) (list (text-of el))))
  '("My title"))


 ;;     Retrieve the text of the <p class="not-me"> tag. (Which doesn't
 ;;     exist.)  Should return '()
 (check-equal?
  (look-down hi-paragraph
			 #:tag 'p
			 #:attr '(class . "not-me")
			 (lambda (el) (list (text-of el))))
  '())

 ;;    Retrieve the text of a 'p' tags that have a class.  Use the
 ;;    #:attr keyword to specify that it must be a <p> with the
 ;;    'class' attr.
 (check-equal?
  (look-down hi-paragraph
			 #:tag 'p
			 #:attr 'class
			 (lambda (el) (text-of el)))
  '("Hello world"))

 ;;    Retrieve the text of the <p class="hi"> tag.  Use the #:attr
 ;;    keyword to specify that it must be a <p> with the 'class'
 ;;    attr that has a value of 'hi'.
 (check-equal?
  (look-down an-html
			 #:tag 'p
			 #:attr '(class . "hi")
			 (lambda (el) (text-of el)))
  '("Hello world"))

 ;;    Retrieve the text of all divs that are of class 'chapter*'.
 ;;    Use the #:attr keyword with a regex to specify that it must be
 ;;    a <div> with the 'class' attr that has a value matching
 ;;    /chapter.*/
 ;;
 (check-equal?
  (look-down an-html
			 #:tag 'div
			 #:attr '(class . #px"^chapter.*")
			 (lambda (el) (text-of el)))
  '("Hello world" "Bye, world"))

 ;;    Retrieve all divs.  Let the action default to 'list' instead of
 ;;    passing it explicitly.
 ;;
 (check-equal?
  (map text-of (look-down an-html
						  #:tag 'div
						  #:attr '(class . #px"^chapter.*")))
  '("Hello world" "Bye, world"))
 )

(test-case
 "html-treebuilder-new"

 (check-equal?
  (html-treebuilder-new an-html-string)
  (html->xexp an-html-string))

;;  (check-equal?
;;   (html-treebuilder-new "./some_HTML-TreeBuilder_test_data.html")
;;   (html->xexp (open-input-file "./some_HTML-TreeBuilder_test_data.html")))

 )

(displayln "Done")
