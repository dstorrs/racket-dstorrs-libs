#lang racket

(require racket/runtime-path
		 net/url
		 (planet neil/html-parsing:3:0)
		 "list-utils.rkt"
		 "web.rkt"
		 "HTML-TreeBuilder.rkt"
		 )

(define latest-filings-url "https://www.sec.gov/cgi-bin/browse-edgar?company=&CIK=&owner=include&count=100&action=getcurrent")

(define latest-13Fs-url "https://www.sec.gov/cgi-bin/browse-edgar?company=&CIK=&type=13F&owner=include&count=100&action=getcurrent")

;;----------------------------------------------------------------------
;;    Get an xexp containing the 'tr's that represent the filings.
;;    Unfortunately, the SEC is using table-based layouts and no ids
;;    or classes.  There are five tables on the page, but only one
;;    div.  We want to get the second table from inside that div.
;;
;; Example of two rows (which together make up one record):
;;   (tr (td "\n") (td "\n") (td (@ (bgcolor "#E6E6E6") (valign "top") (align "left")) (a (@ (href "/cgi-bin/browse-edgar?action=getcompany&amp;CIK=0001529797&amp;owner=include&amp;count=100")) "Clark Gregory S. (0001529797) (Reporting)")) "\n")
;;   (tr (@ (nowrap "nowrap") (valign "top") (align "left")) "\n" (td (@ (nowrap "nowrap")) "4") "\n" (td (@ (nowrap "nowrap")) (a (@ (href "/Archives/edgar/data/1529797/000120919116135387/0001209191-16-135387-index.htm")) "[html]") (a (@ (href "/Archives/edgar/data/1529797/000120919116135387/0001209191-16-135387.txt")) "[text]")) "\n" (td (@ (class "small")) "Statement of changes in beneficial ownership of securities" (br) "Accession Number: 0001209191-16-135387 " (& nbsp) "Act: 34 " (& nbsp) "Size:" (& nbsp) "20 KB\n") "\n" (td (@ (nowrap "nowrap")) "2016-08-03" (br) "21:50:09") "\n" (td (@ (nowrap "nowrap")) "2016-08-03") (td (@ (nowrap "nowrap") (align "left")) (a (@ (href "/cgi-bin/browse-edgar?action=getcompany&amp;filenum=000-17781&amp;owner=include&amp;count=100")) "000-17781") "\n" (br) "161805560"))
(define/contract (filings-raw [url latest-filings-url])
  (->* () ((or/c path-string? url?)) list?)
  (look-down #:tag 'tr
			 (second (look-down #:tag 'table
								(look-down #:tag 'div
										   (get-page url))))))

;;----------------------------------------------------------------------
;;    Fetch the filing rows and process them into a list of hashes for
;;    easy access.  Example hash:
;;
;; '((name                  . "Kuc Paul")
;;   (filed-as              . "Kuc Paul (0001026214) (Filer)";;
;;   (cik                   . "0001026214")
;;   (company-filings-hlink . "http://sec.gov/cgi-bin/browse-edgar?action=getcompany&amp;CIK=0001596946&amp;owner=include&amp;count=100")
;;   (doc-hlink             . "http://sec.gov/Archives/edgar/data/1540922/000114420416116358/0001144204-16-116358-index.htm")
;;  )
;;
(define (filings [url latest-filings-url])
  (define first-hlink-data (compose hlink-data first-hlink))
  ;;
  (define (company-data row)
	(apply append
		   (map list
				'(company-filings-hlink name-filed name cik)
				(append
				 (list (->absolute-url "http://sec.gov" (hlink-url (first-hlink row))))
				 (regexp-match #px"^(.+?)\\s+\\((\\d+)\\)\\s+\\([a-zA-Z ]+\\)$"
							   (hlink-text (first-hlink row)))
				 ))))
  ;;
  (define (doc-data l)
	(list 'doc-hlink
		  (->absolute-url "http://sec.gov"
						  (car (first-hlink-data l))))) ;; Just the URL
  ;;
  (step-by-n (lambda (l)
			   (cond ((null? l)         null)
					 ((atom? l)         (raise (format "Not a list: '~a'" l)))
					 ((not (equal? (length l) 2)) (raise (format "List must have 2 elements, actually has ~a" (length l))))
					 (else (let ((company-name-row (first l))
								 (hlinks-row       (second l)))
							 (apply hash
									(append (doc-data hlinks-row)
											(company-data company-name-row)))))))

			 (cdr (filings-raw url)) ;;Drop header row
			 ))

(provide (all-defined-out))