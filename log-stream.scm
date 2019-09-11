(load "stream-functions.scm")

(define (log-summands n)
  (cons-stream
    (/ 1.0 n)
    (stream-map - (log-summands (+ n 1)))))

(define ln2-stream
  (partial-sums (log-summands 1)))

(define fast-ln2-stream
  (euler-transform ln2-stream))

(define fastest-ln2-stream
  (stram-map
    stream-car
    (make-tableau euler-transform ln2-stream)))

