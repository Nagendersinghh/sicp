; Sets implemented as unordered list
(define (union-set a b)
  (cond ((null? a) b)
	(else
	  (union-set
	    (cdr a)
	    (adjoin-set (car a) b)))))

(define (intersection-set a b)
  (cond ((or (null? a)
	     (null? b))
	 '())
	((element-of-set? (car a) b)
	 (cons (car a) (intersection-set (cdr a) b)))
	(else (intersection-set (cdr a) b))))

(define (element-of-set? item set)
  (cond ((null? set) false)
	((equal? item (car set)) true)
	(else (element-of-set? item (cdr set)))))

(define (adjoin-set item set)
  (cond ((element-of-set? item set) set)
	(else (cons item set))))


