(define (fast-expo base exponent)
  (define (square x) (* x x))

  (define (is-even n) (= (remainder n 2) 0))

  (cond ((= exponent 0) 1)
	((is-even exponent) (square (fast-expo base (/ exponent 2))))
	(else (* base (fast-expo base (- exponent 1))))))


(define (fast-expo-i base exponent)
  (define (square x) (* x x))

  (define (is-even n) (= (remainder n 2) 0))

  (define (exp-iter b n)
    (cond ((= n 1) b)
	  ((is-even n) (exp-iter (square b) (/ n 2)))
	  (else (* base (exp-iter b (- n 1))))))

  (exp-iter base exponent))


