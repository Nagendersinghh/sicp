(define (term n d index total-terms)
  (/
    (n index)
    (+ (d index)
       (if (> total-terms index)
	 (term n d (+ index 1) total-terms)
	 0))))

(define (cont-frac n d k)
  (term n d 1 k))

