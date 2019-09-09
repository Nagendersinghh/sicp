(load "series.scm")

(define (invert-unit-series s)
  (define inverse
    (cons-stream 1 (scale
		     (mul-series inverse (stream-cdr s))
		     -1)))
    inverse)
