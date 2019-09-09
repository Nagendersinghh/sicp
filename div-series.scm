(load "series.scm")
(load "invert-unit-series.scm")

(define (div-series s1 s2)
  (let ((denom (stream-car s2)))
    (if (zero? denom)
	(error "Denominator can not be zero: DIV-SERIES")
	(mul-series
	  (scale s1 denom)
	  (invert-unit-series (scale s2 (/ 1 denom)))))))

