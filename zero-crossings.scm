(define (sign-change-detector new-val old-val)
  (cond ((and (negative? new-val)
	      (not (negative? old-val)))
	 -1)
	((and (not (negative? new-val))
	      (negative? old-val))
	 1)
	(else 0)))

(define (zero-crossings sense-data)
  (stream-map
    sign-change-detector
    sense-data
    (cons-stream 0 sense-data)))

