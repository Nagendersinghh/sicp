(define (interleave s1 s2)
  (if (stream-null? s1)
      s1
      (cons-stream
	(stream-car s1)
	(interleave s2 (stream-cdr s1)))))

