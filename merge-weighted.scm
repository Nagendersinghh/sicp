(define (merge-weighted s1 s2 weight)
  (cond ((null? s1) s2)
	((null? s2) s1)
	(else
	  (let ((w1 (weight (stream-car s1)))
		(w2 (weight (stream-car s2))))
	    (cond ((< w1 w2)
		   (cons-stream
		     (stream-car s1)
		     (merge-weighted (stream-cdr s1)
				     s2
				     weight)))
		  ((> w1 w2)
		   (cons-stream
		     (stream-car s2)
		     (merge-weighted s1
				     (stream-cdr s2)
				     weight)))
		  (else
		    (cons-stream
		      (stream-car s1)
		      (cons-stream
			(stream-car s2)
			(merge-weighted
			  (stream-cdr s1)
			  (stream-cdr s2)
			  weight)))))))))

