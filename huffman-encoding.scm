(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? node)
  (= 'leaf (car node)))

(define (symbol-leaf node) (cadr node))
(define (weight-leaf node) (caddr node))

(define (make-code-tree left right)
  (list left
	right
	(append (symbols left)
		(symbols right))
	(+ (weight left)
	   (weight right))))

(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))

(define (symbols node)
  (if (leaf? node)
    (list (symbol-leaf node))
    (caddr node)))

(define (weight node)
  (if (leaf? node)
    (weight-leaf node)
    (cadddr node)))

(define (decode tree bits)
  (define (decode-1 bits current-branch)
    (if (null? bits)
      '()
      (let ((next-branch
	      (choose-branch
		(car bits)
		current-branch)))
	(if (leaf? next-branch)
	  (cons
	    (symbol-leaf next-branch)
	    (decode-1 tree (cdr bits)))
	  (decode-1 next-branch (cdr bits))))))
  (decode-1 tree bits))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
	((= bit 1) (right-branch branch))
	(else (error "bad bit: CHOOSE-BRANCH" bit))))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
	((< (weight x) (weight (car se)))
	 (cons x set))
	(else
	  (cons (car set)
		(adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
    '()
    (let ((pair (car pairs)))
      (adjoin-set
	(make-leaf (car pair)
		   (cadr pair))
	(make-leaf-set (cdr pairs))))))

(define (encode-symbol symbol tree)
  (define (encode-symbol-1 symbol current-branch)
    (cond ((null? current-branch)
	   (error "Could not find symbol" symbol))
	  ((and (leaf? current-branch)
		(not (eq?
		       (symbol-leaf current-branch)
		       symbol)))
	   (error "Could not find symbol" symbol))
	  ((leaf? current-branch) '())
	  ((element-of-set?
	     symbol
	     (symbols (left-branch current-branch)))
	   (cons 0 (encode-symbol-1
		     symbol
		     (left-branch current-branch))))
	  ((element-of-set?
	     symbol
	     (symbols (right-branch current-branch)))
	   (cons 1 (encode-symbol-1
		     symbol
		     (right-branch current-branch))))))
  (encode-symbol-1 symbol tree))


