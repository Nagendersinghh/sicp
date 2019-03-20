(define (expo base exponent)
  (if (= exponent 0)
    1
    (* base (expo base (- exponent 1)))))

(define (expo_i base exponent)
  (define (expo_helper value count)
    (if (= count 0)
      value
      (expo_helper (* base value) (- count 1))))

  (expo_helper 1 exponent))

