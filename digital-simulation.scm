(load "agenda.scm")

(define (logical-not s)
  (cond ((= s 0) 1)
	((= s 1) 0)
	(else (error " Invalid signal" s))))

(define (logical-and s1 s2)
  (cond ((and (= s1 0) (= s2 0)) 0)
	((and (= s1 1) (= s2 0)) 0)
	((and (= s1 0) (= s2 1)) 0)
	((and (= s1 1) (= s2 1)) 1)
	(else (error "Unknown signal" s1 s2))))

(define (logical-or s1 s2)
  (cond ((and (= s1 0) (= s2 0)) 0)
	((and (= s1 1) (= s2 0)) 1)
	((and (= s1 0) (= s2 1)) 1)
	((and (= s1 1) (= s2 1)) 1)
	(else (error "Error in LOGICAL-OR" s1 s2))))

(define (call-each procedures)
  (if (null? procedures)
      'done
      (begin ((car procedures))
	     (call-each (cdr procedures)))))

(define (make-wire)
  (let ((signal-value 0)
	(action-procedures '()))
    (define (set-my-signal! new-value)
      (if (not (= signal-value new-value))
	  (begin (set! signal-value new-value)
		 (call-each action-procedures))
	  'done))
    (define (accept-action-procedure! proc)
      (set! action-procedures
	(cons proc action-procedures))
      (proc))

    (define (dispatch m)
      (cond ((eq? m 'get-signal)
	     signal-value)
	    ((eq? m 'set-signal!)
	     set-my-signal!)
	    ((eq? m 'add-action!)
	     accept-action-procedure!)
	    (else (error "Unknown operation: WIRE" m))))
    dispatch))

(define (get-signal wire)
  (wire 'get-signal))

(define (set-signal! wire new-value)
  ((wire 'set-signal!) new-value))

(define (add-action! wire action-procedure)
  ((wire 'add-action!) action-procedure))

(define (inverter input output)
  (define (invert-input)
    (let ((new-value (logical-not (get-signal input)))
	  (inverter-delay 2))
      (after-delay
	inverter-delay
	(lambda ()
	  (set-signal! output new-value)))))
  (add-action! input invert-input)
  'ok)

(define (and-gate input1 input2 output)
  (define (proc)
    (let ((new-value (logical-and
		       (get-signal input1)
		       (get-signal input2)))
	  (and-delay 3))
      (after-delay
	and-delay
	(lambda ()
	  (set-signal! output new-value)))))

  (add-action! input1 proc)
  (add-action! input2 proc)
  'ok)

(define (or-gate input1 input2 output)
  (define (proc)
    (let ((new-value (logical-or
		       (get-signal input1)
		       (get-signal input2)))
	  (or-delay 5))
      (after-delay
	or-delay
	(lambda ()
	  (set-signal! output new-value)))))
  (add-action! input1 proc)
  (add-action! input2 proc)
  'ok)

;; OR gate using AND and NOT gate
(define (or-gate-compund input1 intput2 output)
  (let ((a-star (make-wire))
	(b-star (make-wire))
	(s (make-wire)))
    (inverter a a-star)
    (inverter b b-star)
    (and-gate a-star b-star s)
    (inverter s output))
  'ok)

(define (half-adder a b s c)
  (let ((d (make-wire)) (e (make-wire)))
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    'ok))

(define (full-adder a b c-in sum c-out)
  (let ((c1 (make-wire))
	(c2 (make-wire))
	(s (make-wire)))
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c1 c2 c-out)
    'ok))

(define (after-delay delay action)
  (add-to-agenda!
    (+ delay (current-time the-agenda))
    action
    the-agenda))

(define (propagate)
  (if (empty-agenda? the-agenda)
      'done
      (let ((first-item
	      (first-agenda-item the-agenda)))
	(first-item)
	(remove-first-agenda-item! the-agenda)
	(propagate))))

;; A `probe` on the wire that shows its value
;; and name.
(define (probe name wire)
  (add-action!
    wire
    (lambda()
      (newline)
      (display name)
      (display " ")
      (display (current-time the-agenda))
      (display " New-value = ")
      (display (get-signal wire)))))

;; The agenda to use for the simulation
(define the-agenda (make-agenda))

