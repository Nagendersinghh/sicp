(define (make-queue)
  (cons '() '()))

(define (front-ptr q) (car q))
(define (rear-ptr q) (cdr q))

(define (empty-queue? q)
  (null? (front-ptr q)))

(define (front-queue q)
  (if (empty-queue? q)
      (error "Queue is empty" q)
      (car (front-ptr q))))

(define (set-front-ptr! q item)
  (set-car! q item))
(define (set-rear-ptr! q item)
  (set-cdr! q item))

(define (insert-queue q item)
  (let ((new-pair (cons item '())))
    (cond ((empty-queue? q)
	   (set-front-ptr! q new-pair)
	   (set-rear-ptr! q new-pair)
	   q)
	  (else (set-cdr! (rear-ptr q) new-pair)
		(set-rear-ptr! q new-pair)
		q))))

(define (delete-queue q)
  (cond ((empty-queue? q)
	 (error "DELETE! called with an empty queue" q))
	(else (set-front-ptr!
		q
		(cdr (front-ptr q)))
	      q)))

(define (print-queue q)
  (define (iter front)
    (if (null? front)
	(newline)
	(begin (display (car front))
	       (display " ")
	       (iter (cdr front)))))
    (iter (front-ptr q)))




