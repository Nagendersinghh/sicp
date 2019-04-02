(define (variable? expr) (symbol? expr))

(define (same-variable? v1 v2)
  (and (variable? v1)
       (variable? v2)
       (eq? v1 v2)))

(define (sum? expr)
  (and (pair? expr)
       (eq? (car expr) '+)))

(define (addend expr) (cadr expr))

(define (augend expr)
  (let ((ending-terms (cddr expr)))
    (if (null? (cdr ending-terms))
      (car ending-terms)
      (cons '+ ending-terms))))

(define (product? s)
  (and (pair? s)
       (eq? (car s) '*)))

(define (make-product s1 s2)
  (cond ((or (=number? s1 0) (=number? s2 0)) 0)
	((=number? s1 1) s2)
	((=number? s2 1) s1)
	((and (number? s1) (number? s2)) (* s1 s2))
	(else (list '* s1 s2))))

(define (make-sum s1 s2)
  (cond ((=number? s1 0) s2)
	((=number? s2 0) s1)
	((and (number? s1) (number? s2)) (+ s1 s2))
	(else (list '+ s1 s2))))

(define (multiplier s) (cadr s))

(define (multiplicand s)
  (let ((ending-terms (cddr s)))
    (if (null? (cdr ending-terms))
      (car ending-terms)
      (cons '* ending-terms))))

(define (exponentiation? expr)
  (and (pair? expr) (eq? (car expr) '**)))

(define (base s) (cadr s))
(define (exponent s) (caddr s))

(define (make-exponentiation s1 s2)
  (cond ((=number? s1 0) 0)
	((=number? s1 1) 1)
	((=number? s2 0) 1)
	((=number? s2 1) s1)
	(else (list '** s1 s2))))

(define (deriv expr var)
  (cond ((number? expr) 0)
	((variable? expr)
	 (if (same-variable? expr var) 1 0))
	((sum? expr)
	 (make-sum (deriv (addend expr) var)
		   (deriv (augend expr) var)))
	((product? expr)
	 (make-sum (make-product (multiplier expr)
				 (deriv (multiplicand expr) var))
		   (make-product (multiplicand expr)
				 (deriv (multiplier expr) var))))
	((exponentiation? expr) (make-product (exponent expr)
					      (make-product
						(make-exponentiation
						  (base expr)
						  (- (exponent expr) 1))
						(deriv (base expr) var))))
	(else (error "unknown expression type: DERVI" expr))))

