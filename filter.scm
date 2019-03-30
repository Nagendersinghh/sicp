(load "prime.scm")

(define (my-filter predicate seq)
  (cond ((null? seq) ())
	((predicate (car seq))
	 (cons (car seq) (my-filter predicate (cdr seq))))
	(else (my-filter predicate (cdr seq)))))

(define (my-accumulate op initial seq)
  (if (null? seq)
    initial
    (op (car seq)
	(accumulate op
		     initial
		     (cdr seq)))))

(define (my-map p sequence)
  (accumulate (lambda (x y) (cons (p x) y))
	      ()
	      sequence))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
    ()
    (cons (my-accumulate op init (my-map
				(lambda (x) (car x))
				seqs))
	  (accumulate-n op init (my-map
				  (lambda (x) (cdr x))
				  seqs)))))

(define (dot-product a b)
  (my-accumulate + 0 (accumulate-n * 1 (list a b))))

(define (enumerate-interval a b)
  (define (inner-helper a b)
    (if (= a b)
      (cons a ())
      (cons a (inner-helper (+ a 1) b))))
  (inner-helper (min a b) (max a b)))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (prime-sum-pairs n)
  (filter prime-sum?  (unique-pairs n)))

(define (my-flatmap proc seq)
  (accumulate append () (my-map proc seq)))

(define (permutations s)
  (if (null? s)
    (list ())
    (my-flatmap
      (lambda (x) (map
		    (lambda (p) (cons x p))
		    (permutations (remove x s))))
      s)))

(define (remove item seq)
  (filter (lambda (x) (not (= item x))) seq))

(define (unique-pairs n)
  (my-flatmap
    (lambda (i) (my-map
		  (lambda (j) (list i j))
		  (enumerate-interval 1 (- i 1))))
    (enumerate-interval 2 n)))

(define (triplets n)
  (my-flatmap (lambda (i)
		(my-flatmap (lambda (j)
			      (my-map (lambda (k)
					    (list i j k))
					  (enumerate-interval 1 n)))
			    (enumerate-interval 1 n)))
	      (enumerate-interval 1 n)))

(define (distinct-members? s)
  (unique? s))

(define (unique? s)
  (define (helper seq)
    (cond ((null? seq) #t)
	  ((null? (cdr seq)) #t)
	  ((= (car seq) (car (cdr seq))) #f)
	  (else (helper (cdr seq)))))

  (helper (sort s <)))

(define (sum? total s)
  (= total (my-accumulate + 0 s)))

(define (triplet-sums s n)
  (filter
    (lambda (triplet) (sum? s triplet))
    (filter distinct-members?
	    (triplets n))))

