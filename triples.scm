(load "pairs.scm")

(define (triples s t u)
  (let ((pp (partial-pairs t u)))
    (cons-stream
      (cons (stream-car s) (stream-car pp))
      (interleave 
	(stream-map
	  (lambda (x) (cons (stream-car s) x))
	  (stream-cdr pp))
	(triples
	  (stream-cdr s)
	  (stream-cdr t)
	  (stream-cdr u))))))

