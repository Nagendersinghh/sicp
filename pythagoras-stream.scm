(load "integers.scm")
(load "triples.scm")

(define pythagoras
  (stream-filter
    (lambda (triplet)
      (let ((first (car triplet))
	    (second (cadr triplet))
	    (third (caddr triplet)))
	(and (< first second)
	     (= (+ (square first)
		   (square second))
		(square third)))))
    (triples integers integers integers)))

