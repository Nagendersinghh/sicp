(load "sieve.scm")
(load "integers.scm")

(define primes (sieve (integers-starting-from 2)))
