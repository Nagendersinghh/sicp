(define (fermat-test n)
  (define (square x) (* x x))

  (define (expmod base exponent m)
    (cond ((= exponent 0) 1)
	  ((even? exponent)
	   (remainder
	     (square (expmod base (/ exponent 2) m))
	     m))
    (else
      (remainder
	(* base (expmod base (- exponent 1) m))
	m))))

  (define (try-it a) (= (expmod a n n) a))

  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
	((fermat-test n) (fast-prime? n (- times 1)))
	(else false)))

