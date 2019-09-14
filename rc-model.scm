(load "summer.scm")

(define (rc-model r c dt)
  (lambda (init-voltage current-stream)
    (add-streams
      (scale
	(integral current-stream init-voltage dt)
	(/ 1 c))
      (scale current-stream r))))

