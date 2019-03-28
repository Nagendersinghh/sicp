(define (cons a b)
  (* (expt 2 a)
     (expt 3 b)))

(define (num-divs target divisor count)
  (if (not (= (remainder target divisor) 0))
    count
    (num-divs (/ target divisor) divisor (+ count 1))))

(define (car z)
  (num-divs z 2 0))

(define (cdr z)
  (num-divs z 3 0))

