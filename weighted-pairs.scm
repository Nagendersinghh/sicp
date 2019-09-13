(load "merge-weighted.scm")

(define (weighted-pairs s t weight)
  (cons-stream
    (list (stream-car s) (stream-car t))
    (merge-weighted
      (stream-map (lambda (x) (list (stream-car t) x))
		  (stream-cdr s))
      (weighted-pairs (stream-cdr s)
		      (stream-cdr t)
		      weight)
      weight)))

