(load "monte-carlo.scm")

(define (random-in-range low high)
  (let ((range (- high low)))
	(+ low (random range))))

; This is probably incorrect. I moved on without correcting it.
; Correct this if time permits.
(define (estimate-integral predicate x1 y1 x2 y2 trials)
  (let ((area (* (- x2 x1) (- y2 y1)))
	(frac (monte-carlo
		trials
		(lambda ()
		  (let ((x (random-in-range x1 x2))
			(y (random-in-range y1 y2)))
		    (predicate x y))))))
    (* frac area)))

