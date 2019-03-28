(define (make-rat n d)
  (let ((g (gcd n d)))
    (cond ((and
	     (positive? n)
	     (positive? d))
	   (cons (/ (abs n) g) (/ (abs d) g)))
	  ((and
	     (negative? n)
	     (negative? d))
	   (cons (/ (abs n) g) (/ (abs d) g)))
	  (else (cons (* (/ (abs n) g) -1) (/ (abs d) g))))))

(define (numer x) (car x))
(define (denom x) (cdr x))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
	       (* (numer y) (denom x)))
	    (* (denom x) (denom y))))
(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
	       (* (numer y) (denom x)))
	    (* (denom x) (denom y))))
(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
	    (* (denom x) (denom y))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

