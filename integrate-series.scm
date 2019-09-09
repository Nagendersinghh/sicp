(load "stream.scm")
(load "integers.scm")
(load "ones-stream.scm")

(define (integrate-series s)
  (stream-map * s (stream-map / ones integers)))
