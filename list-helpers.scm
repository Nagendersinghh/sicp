(define (item-at index seq)
  (cond ((null? seq) (error "Can't access Empty list"))
	((= index 0) (car seq))
	(else (item-at (- index 1) (cdr seq)))))

(define (update index value seq)
  (if (= index 0)
    (cons value (cdr seq))
    (cons (car seq) (update (- index 1) value (cdr seq)))))

