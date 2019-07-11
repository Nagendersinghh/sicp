;; A multi dimensional table
(define (make-table same-key?)
  (let ((local-table (list 'table)))
    (define (assoc key records)
      (cond ((null? records) false)
	    ((same-key? key (caar records)) (car records))
	    (else (assoc key (cdr records)))))

    (define (_lookup keys table)
      (let ((subtable (assoc (car keys)
			     (cdr table))))
	(if subtable
	    (cond ((null? (cdr keys)) subtable) ;; Just found the final key, return the subtable
		  ((list? (cdr table)) ;; Has further subtables to search
		   (_lookup (cdr keys) subtable)))
	    false)))

    (define (lookup keys)
      (if (null? keys)
	  false
	  (let ((record (_lookup keys local-table)))
	   (if record
	       (cdr record)
	       false))))

    (define (_insert! keys value table)
      (let ((subtable (assoc (car keys) (cdr table))))
	(if subtable
	    (cond ((null? (cdr keys))
		   (set-cdr! subtable value))
		  ((not (list? (cdr subtable)))
		   (set-cdr! subtable
			     (list (cons (caar keys) ())))
		   (_insert! (cdr keys) value subtable))
		  (else (_insert! (cdr keys) value subtable)))
	    (cond ((null? (cdr keys))
		   (set-cdr! table
			     (cons (cons (car keys) value)
				   (cdr table))))
		  (else (begin (set-cdr! table
					 (cons (cons (car keys) ())
					       (cdr table)))
			       (_insert! keys value table))))))
      'ok)

    (define (insert! keys value)
      (if (null? keys)
	  'ok
	  (_insert! keys value local-table)))

    (define (dispatch m)
      (cond ((eq? m 'lookup) lookup)
	    ((eq? m 'insert!) insert!)
	    (else (error "Unknown operation on TABLE\n"))))
    dispatch))

