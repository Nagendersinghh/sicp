(define (partial-sums s)
  (define (inner-helper element stream)
    (let ((result (+ element (stream-car stream))))
      (cons-stream
	result
	(inner-helper result (stream-cdr stream)))))
  (inner-helper 0 s))

