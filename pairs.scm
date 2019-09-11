(load "interleave.scm")

; The diagonal and above items
(define (partial-pairs s t)
  (cons-stream
    (list (stream-car s) (stream-car t))
    (interleave
      (stream-map
	(lambda (x)
	  (list (stream-car s) x))
	(stream-cdr t))
      (pairs (stream-cdr s) (stream-cdr t)))))

(define (pairs s t)
  (cons-stream
    (list (stream-car s) (stream-car t))
    (interleave
      (interleave
	(stream-map
	  (lambda (x)
	    (list (stream-car s) x))
	  (stream-cdr t))
	(stream-map
	  (lambda (x)
	    (list x (stream-car t)))
	  (stream-cdr s)))
      (pairs (stream-cdr s) (stream-cdr t)))))

; Louis Reasoner
(define (loose-pairs s t)
  (interleave
    (stream-map
      (lambda (x) (list (stream-car s) x))
      t)
    (loose-pairs (stream-cdr s)
	   (stream-cdr t))))

