(load "stream.scm")

(define (integral integrand init dt)
  (define int
    (cons-stream
      init
      (add-streams (scale integrand dt)
		   int)))
  int)

