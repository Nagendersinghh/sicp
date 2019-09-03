(define (make-semaphore size)
  (let ((cell (list size)))
    (define (dispatch m)
      (cond ((eq? m 'acquire)
	     (if (test-and-set! cell)
		 (dispatch 'acquire)))
	    ((eq? m 'release)
	     (clear! cell))))
    dispatch))

(define (test-and-set! cell)
  (if (eq? (car cell) 0)
      true
      (begin (set-car! cell (- (car cell) 1))
	     false)))

(define (clear! cell)
  (set-car! cell (+ (car cell) 1)))
