(load "weighted-pairs.scm")

(define (cube x) (* x x x))

(define (cubic-wt p) (+ (cube (car p)) (cube (cadr p))))

(define cubic-stream
  (weighted-pairs integers integers cubic-wt))

(define (ramanujan s)
  (let ((pair1 (stream-car s))
	(pair2 (stream-car (stream-cdr s))))
    (if (= (cubic-wt pair1) (cubic-wt pair2))
	(cons-stream
	  (cubic-wt pair1)
	  (ramanujan (stream-cdr s)))
	(ramanujan (stream-cdr s)))))

(define ramanujan-stream (ramanujan cubic-stream))

