; Set implemented as a tree

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))

(define (make-tree entry left-branch right-branch)
  (list entry left-branch right-branch))

(define (element-of-set? item set)
  (cond ((null? set) false)
	((= item (entry set)) true)
	((< item (entry set))
	 (element-of-set? item (left-branch set)))
	((> item (entry set))
	 (element-of-set? ite (right-branch set)))))

(define (adjoin-set item set)
  (cond ((null? set) (make-tree item () ()))
	((= item (entry set)) set)
	((< item (entry set))
	 (make-tree (entry set)
		    (adjoin-set item (left-branch set))
		    (right-branch set)))
	(else (make-tree (entry set)
			 (left-branch set)
			 (adjoin-set
			   item
			   (right-branch set))))))
