(define dx 0.00001)

(define (deriv f)
  (lambda (x)
    (/ (- (f (+ x dx)) (f x)) dx)))

