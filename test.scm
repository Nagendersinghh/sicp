(load "constraint-system.scm")

(define a1 (make-connector))
(define a2 (make-connector))

(define m1 (make-connector))
(define m2 (make-connector))
(define p (make-connector))

(define s (make-connector))

(adder a1 a2 s)

(multiplier m1 m2 p)
