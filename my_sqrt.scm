(define (my_sqrt x)
  (define (square x) (* x x))
  (define (good-enough guess y)
    (< (abs (- (square guess) y)) 0.001))

  (define (improve guess y)
    (avg guess (/ y guess)))

  (define (sqrt-iter guess y)
    (if (good-enough guess y)
      guess
      (sqrt-iter (improve guess y) y)))

  (define (avg a b)
    (/ (+ a b) 2))

  (sqrt-iter 1.0 x))





