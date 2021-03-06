(define (assoc key records)
  (cond ((null? records) false)
	((equal? key (caar records)) (car records))
	(else (assoc key (cdr records)))))

(define (lookup key-1 key-2 table)
  (let ((subtable (assoc key-1 (cdr table))))
    (if subtable
	(let ((record (assoc key-2 (cdr subtable))))
	  (if record (cdr record) false))
	false)))

(define (insert! key-1 key-2 value table)
  (let ((subtable (assoc key-1 (cdr table))))
    (if subtable
	(let ((record (assoc key-2 (cdr subtable))))
	  (if record
	      (set-cdr! value record)
	      (set-cdr! table
			(cons (cons key-2 value)
			      (cdr subtable)))))
	(set-cdr!
	  table
	  (cons (list key-1 (cons key-2 value))
		(cdr table)))))
  'ok)

(define (make-table)
  (list '*table*))


