(load "zero-stream.scm")
(load "stream.scm")

(define (mul-series s1 s2)
  (let ((first1 (stream-car s1))
	(first2 (stream-car s2)))
    (cons-stream (* first1 first2)
		 (add-streams
		   (mul-series (cons-stream first1 zeros)
				(stream-cdr s2))
		   (mul-series (stream-cdr s1) s2)))))

