(define (cube x) (* x x x))

(define (p x) (- (* 3 x) (* 4 (cube x))))

(define (sine angl)
  (if (not (> (abs angl) 0.1))
  angl
  (p (sine (/ angl 3.0)))))

