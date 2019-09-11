(define (average a b) (/ (+ a b) 2))

(define (sqrt-improve guess x)
  (average guess (/ x guess)))

(define (sqrt-stream x)
  (define guesses
    (cons-stream
      1.0
      (stream-map
	(lambda (guess)
	  (sqrt-improve guess x))
	guesses)))
  guesses)

