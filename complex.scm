(define (add-complex z1 z2)
  (make-from-real-imag
    (+ (real z1) (real z2))
    (+ (imag z1) (imag z2))))

(define (sub-complex z1 z2)
  (make-from-real-imag
    (- (real z1) (real z2))
    (- (imag z1) (real z2))))

(define (mul-complex z1 z2)
  (make-from-mag-angle
    (* (mag z1) (mag z2))
    (+ (ang z1) (ang z2))))

(define (div-complex z1 z2)
  (make-from-mag-angle
    (/ (mag z1) (mag z2))
    (- (ang z1) (ang z2))))

(define (attach-tag type-tag contents)
  (cons type-tag contents))

(define (type-tag datum)
  (if (pair? datum)
    (car datum)
    (error "Bad tagged datum: TYPE-TAG" datum)))

(define (contents datum)
  (if (pair? datum)
    (cdr datum)
    (error "Bad tagged datum: CONTENTS" datum)))

(define (rectangular? z)
  (eq? (type-tag z) 'rectangular))

(define (polar? z)
  (eq? (type-tag z) 'polar))

; Generic representation
(define (real z)
  (cond ((rectangular? z) (real-rectangular (contents z)))
	((polar? z) (real-polar (contents z)))
	(else (error "Unknown type: REAL" z))))
(define (imag z)
  (cond ((rectangular? z) (imag-rectangular (contents z)))
	((polar? z) (imag-polar (contents z)))
	(else (error "Unknown type: IMAG" z))))
(define (mag z)
  (cond ((rectangular? z) (mag-rectangular (contents z)))
	((polar? z) (mag-polar (contents z)))
	(else (error "Unknown type: MAG" z))))
(define (ang z)
  (cond ((rectangular? z) (ang-rectangular (contents z)))
	((polar? z) (ang-polar (contents z)))
	(else (error "Unknownn type: ANG" z))))
(define (make-from-real-imag x y)
  (make-from-real-imag-rectangular x y))
(define (make-from-mag-angle r a)
  (make-from-mag-angle-polar r a))

; Rectangular representation
(define (install-rectangular-package)
  ;; Internal procedures
  (define (make-from-real-imag x y) (cons x y))
  (define (real z) (car z))
  (define (imag z) (cdr z))
  (define (mag z)
    (sqrt (+ (square (real z))
	     (square (imag z)))))
  (define (ang z) (atan (imag z) (real z)))
  (define (make-from-mag-angle r a)
    (cons (* r (cos a)) (* r (sin a))))

  ;; Interface to the rest of the system
  (define (tag x)
    (attach-tag 'rectangular x))
  (put 'real '(rectangular) real)
  (put 'imag '(rectangular) imag)
  (put 'mag '(rectangular) mag)
  (put 'ang '(rectangular) ang)

  (put 'make-rom-mag-angle
       '(rectangular)
       (lambda (r a)
	 (tag (make-from-mag-angle r a))))

  (put 'make-from-real-imag
       '(rectangular)
       (lambda (x y)
	 (tag (make-from-real-imag x y))))
  'done)

(define (install-polar-package)
  ;; Internal procedures
  (define (make-from-mag-angle r a) (cons r a))
  (define (mag z) (car z))
  (define (ang z) (cdr z))
  (define (real z)
    (* (mag z) (cos (ang z))))
  (define (imag z)
    (* (mag z) (sin (ang z))))
  (define (make-from-real-imag x y)
    (cons (sqrt (+ (square x) (square y)))
	  (atan y x)))
  ;; Interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))
  (put 'mag '(polar) mag)
  (put 'ang '(polar) ang)
  (put 'real '(polar) real)
  (put 'imag '(polar) imag)
  (put 'make-from-mag-angle
       '(polar)
       (lambda (r a) (tag (make-from-mag-angle r a))))
  (put 'make-from-real-imag
       '(polar)
       (lambda (x y) (tag (make-from-real-imag x y))))
  'done)

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
	(apply proc (map contents args))
	(error
	  "No method for these types: APPLY-GENERIC"
	  (list op type-tags))))))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
	(apply proc (map contents args))
	(if (= (length args) 2)
	  (let ((type1 (car type-tags))
		(type2 (cadr type-tags))
		(a1 (car args))
		(a2 (cadr args)))
	    (let ((t1->t2 (get-coercion type1 type2))
		  (t2->t1 (get-coercion type2 type1)))
	      (cond (t1->t2
		      (apply-generic op (t1->t2 a1) a2))
		    (t2->t1
		      (apply-generic op a1 (t2->t1 a2)))
		    (else
		      (error "No method for these types"
			     (list op type-tags))))))
	  (error
	    "No method for these types"
	    (list op type-tags)))))))


