(define (pascal row col)
  (cond ((< row col) -1)
	((or (= col 0)
	     (= col row))
	 1)
	(else (+
		(pascal (- row 1) (- col 1))
		(pascal (- row 1) col)))))

