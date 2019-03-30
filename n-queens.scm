(load "list-helpers.scm")

(define (my-accumulate op initial seq)
  (if (null? seq)
    initial
    (op (car seq)
	(my-accumulate op
		     initial
		     (cdr seq)))))

(define (enumerate-interval a b)
  (define (inner-helper a b)
    (if (= a b)
      (cons a ())
      (cons a (inner-helper (+ a 1) b))))
  (inner-helper (min a b) (max a b)))

(define (my-flatmap proc seq)
  (my-accumulate append () (map proc seq)))

(define (make-position row col)
  (cons row col))
(define (position-row position)
  (car position))
(define (position-column position)
  (cdr position))

(define (queens board-size)
  (define (place-queens k)
    (if (= k 0)
      (list ())
      (filter (lambda (placement) (safe? placement k))
	      (add-column board-size k (place-queens (- k 1))))))

  (place-queens board-size))

(define (put-queen placement row col)
  (append placement (list (make-position row col))))

(define (add-column board-size col placements)
  (my-flatmap (lambda (placement)
		(map (lambda (row)
		       (put-queen placement row col))
		     (enumerate-interval 1 board-size)))
	      placements))

(define (safe? placement col)
  (let ((cur-queen (find (lambda (pos) (= (position-column pos) col))
			 placement))
	(other-queens (filter (lambda (pos) (not (= (position-column pos) col)))
			      placement)))
    (define (attacks? q1 q2)
      (or (= (position-row q1) (position-row q2))
	  ;  (= (abs (- (position-row q1) (position-row q2)))
	  ;    (abs (- (position-column q2) (position-column q2))))))
	  (= (- (position-row q1) (position-column q1))
	     (- (position-row q2) (position-column q2)))
	  (= (+ (position-row q1) (position-column q1))
	     (+ (position-row q2) (position-column q2)))))
    (define (iter q board)
      (or (null? board)
	  (and (not (attacks? q (car board)))
	       (iter q (cdr board)))))
    (iter cur-queen other-queens)))

