(define (gcd-euclid a b)
  (if (= b 0)
    a
    (gcd-euclid b (remainder a b))))

