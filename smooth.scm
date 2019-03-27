; Smooth a function

(define (smooth f)
  (lambda (x) (/ (+
		   (f (- x dx))
		   (f x)
		   (f (+ x dx)))
		 3)))

