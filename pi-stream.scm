(load "stream-functions.scm")
(load "stream.scm")

" π = 1 - 1/3 + 1/5 - 1/7 ..." 
(define (pi-summands n)
  (cons-stream
    (/ 1.0 n)
    (stream-map - (pi-summands (+ n 2)))))

(define pi-stream
  (scale (partial-sums (pi-summands 1)) 4))

