(define (monte-carlo trials experiment)
  (define (iter trials-attempted trials-passed)
    (cond ((= (- trials trials-attempted) 0)
	   (/ trials-passed trials))
	  ((experiment)
	   (iter (+ trials-attempted 1) (+ trials-passed 1)))
	  (else
	    (iter (+ trials-attempted 1) trials-passed))))
  (iter 0 0))


