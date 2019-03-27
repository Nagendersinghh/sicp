(load "deriv.scm")
(load "fixed-point.scm")

(define (newton-transform f)
  (lambda (x)
    (- x (/ (f x)
	    ((deriv f) x)))))

(define (newtons-method f guess)
  (fixed-point (newton-transform f)
	       guess))

