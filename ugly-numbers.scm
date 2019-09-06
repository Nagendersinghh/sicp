(load "stream-merge.scm")
(load "stream.scm")

(define ugly-numbers
  (cons-stream 1 (merge (scale ugly-numbers 2)
			(merge (scale ugly-numbers 3)
			       (scale ugly-numbers 5)))))

