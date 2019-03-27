(load "compose.scm")

(define (repeated f n)
  (define (apply-repeated f n)
    (if (= n 1)
      f
      (compose f (apply-repeated f (- n 1)))))
  (apply-repeated f n))

