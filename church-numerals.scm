(define zero (lambda (f) (lambda (x) x)))
(define one (lambda (f) (lambda (x) (f x))))

(define (pp a)
  ((a (lambda (val) (+ val 1))) 0))

(define (add a b)
  (lambda (f) (lambda (x) ((a f) ((b f) x)))))

