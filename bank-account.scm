(load "serializer.scm")

(define (make-account balance)
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)

  (define (withdraw amount)
    (if (>= balance amount)
	(set! balance (- balance amount))
	balance))
  (let ((protected (make-serializer)))
    (define (dispatch m)
      (cond ((eq? m 'withdraw)
	     (protected withdraw))
	    ((eq? m 'deposit)
	     (protected deposit))
	    ((eq? m 'balance)
	     balance)
	    (else (error "Unknown request: MAKE-ACCOUNT" m))))
    dispatch))

  
