(define (make-mutex)
  (let ((cell (list false)))
    (define (dispatch m)
      (cond ((eq? m 'acquire)
	     (if (test-and-set! cell)
		 (dispatch 'acquire)))
	    ((eq? m 'release) (clear! cell))))
    dispatch))

(define (clear! cell) (set-car! cell false))

(define (test-and-set! cell)
  (if (car cell)
      true
      (begin (set-car! cell true)
	     false)))
