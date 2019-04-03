; Sets implemented as ordered lists
(define (element-of-set? item set)
  (cond ((null? set) false)
	((> item (car set)) false)
	((= item (car set)) true)
	(else (element-of-set? item (cdr set)))))

(define (intersection-set a b)
  (if (or (null? a)
	  (null? b))
    '()
    (let ((x1 (car a))
	  (x2 (car b)))
      (cond ((= x1 x2)
	     (cons x1 (intersection-set
			(cdr a)
			(cdr b))))
	    ((< x1 x2)
	     (intersection-set (cdr a) b))
	    ((> x1 x2)
	     (intersection-set a (cdr b)))))))

(define (union-set a b)
  (cond ((null? a) b)
	((= (car a) (car b))
	 (cons
	   (car a)
	   (union-set (cdr a) (cdr b))))
	((> (car a) (car b))
	 (cons
	   (car b)
	   (union-set a (cdr b))))
	((< (car a) (car b))
	 (cons
	   (car a)
	   (union-set (cdr a) b)))))

(define (adjoin-set item set)
  (cond ((element-of-set? item set) set)
	((null? set) (list item))
	((> (car set) item) (cons item set))
	(else (cons
		(car set)
		(adjoin-set item (cdr set))))))

