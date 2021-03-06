(define (stream-car stream)
  (car stream))
(define (stream-cdr stream)
  (force (cdr stream)))

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
	(apply proc (map stream-car argstreams))
	(apply stream-map
	  (cons proc (map stream-cdr argstreams))))))

(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin
	(proc (stream-car s))
	(stream-for-each proc (stream-cdr s)))))

(define (display-stream s)
  (stream-for-each display-line s))

(define (display-stream-upto s count)
  (cond ((or (stream-null? s)
	     (zero? count))
	 'done)
	((> count 0)
	 (begin
	   (display-line (stream-car s))
	   (display-stream-upto
	     (stream-cdr s)
	     (- count 1))))
	(else (display-stream-upto
		(stream-cdr s)
		(- count 1)))))

(define (display-line x)
  (newline)
  (display x))

(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream low (stream-enumerate-interval
			 (+ low 1)
			 high))))

(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
	((pred (stream-car stream))
	 (cons-stream
	   (stream-car stream)
	   (stream-filter
	     pred
	     (stream-cdr stream))))
	(else (stream-filter pred (stream-cdr stream)))))

(define (show x)
  (display-line x)
  x)

(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define (mul-streams s1 s2)
  (stream-map * s1 s2))

(define (scale s factor)
  (stream-map (lambda (item) (* factor item)) s))

