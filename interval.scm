; Interval arithmetic
(define (add-interval x y)
  (make-interval (+ (lower-bound x)
		    (lower-bound y))
		 (+ (upper-bound x)
		    (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x)
	       (lower-bound y)))
	(p2 (* (lower-bound x)
	       (upper-bound y)))
	(p3 (* (upper-bound x)
	       (lower-bound y)))
	(p4 (* (upper-bound x)
	       (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
		   (max p1 p2 p3 p4))))

(define (spans-zero? a)
  (not (or (and (positive? (lower-bound a))
	   (positive? (upper-bound a)))
	   (and (negative? (lower-bound a))
		(negative? (upper-bound a))))))


(define (div-interval x y)
  (if (spans-zero? x)
    (error "Can't divide an interval that spans zero")
    (mul-interval x
		  (make-interval
		    (/ 1.0 (upper-bound y))
		    (/ 1.0 (lower-bound y))))))

(define (sub-interval x y)
  (add-interval x (mul-interval y (make-interval -1 -1)))) 

(define (make-interval a b) (cons (min a b) (max a b)))
(define (lower-bound a) (car a))
(define (upper-bound b) (cdr b))
