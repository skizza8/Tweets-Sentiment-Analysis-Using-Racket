#lang racket
(require data-science-master)
(require csv-reading)
(require plot)
(require math)

(define file-path "output_nov.csv")

(define (read-csv-with-pipe-delimiter file-path
		  #:->number? [->number? #f]
		  #:header? [header? #t]
                  )
  (let ((csv-reader (make-csv-reader-maker
                     '((separator-chars #\|)
                       (comment-chars #\#))))) 
    (with-input-from-file file-path
      (lambda ()
	(let* ((tmp (csv->list (csv-reader (current-input-port)))))
	  (if ->number?
	      ;; try to convert everything to numbers rather than
	      ;; strings. This should be made smarter, converting only
	      ;; those columns which are actually numbers
	      (if header?
		  (cons (car tmp) (map (lambda (x) (map string->number x)) (cdr tmp)))
		  (map (lambda (x) (map string->number x)) tmp))
	      ;; Else, leave everything as strings
	      tmp))))));; change read-csv-with-pipe-delimiter



(define data (read-csv-with-pipe-delimiter file-path 
                       
                       )
  )

(define filtered_data 
  (let ([tmp (map (λ (x) (list (list-ref x 0))) data)])
    (filter (λ (x) (not (string-prefix? (first x) "RT"))) tmp))) ;; remove all retweets

(define lst_filtered_data (flatten filtered_data)) 

(define tweet-text (apply string-append lst_filtered_data)) 


(define cleaned-tweet-text (string-normalize-spaces
		   (remove-punctuation
		    (string-downcase tweet-text) #:websafe? #t))) 



(define words (document->tokens cleaned-tweet-text #:sort? #t)) 


(define sentiment (list->sentiment words #:lexicon 'nrc)) 

 
(aggregate sum ($ sentiment 'sentiment) ($ sentiment 'freq))

(let ([counts (aggregate sum ($ sentiment 'sentiment) ($ sentiment 'freq))])
  (parameterize ((plot-width 1000))
    (plot (list
	   (tick-grid)
	   (discrete-histogram
	    (sort counts (λ (x y) (> (second x) (second y))))
	    #:color "Orange"
	    #:line-color "Black"))
	  #:x-label "Mood of Tweets"
	  #:y-label "Frequency"))) 

(define sentiment_bing (list->sentiment words #:lexicon 'bing))
(parameterize ([plot-height 500])
  (plot (discrete-histogram
	 (aggregate sum ($ sentiment_bing 'sentiment) ($ sentiment_bing 'freq))
	 #:y-min 0
	 #:y-max 100000
	 #:invert? #t
	 #:color "Orange"
	 #:line-color "Black")
	#:x-label "Frequency"
	#:y-label "Sentiment Polarity")) 