#lang racket

(define (primes max-n)
  (define (iter n old-primes)
    (cond
      ((>= n max-n) old-primes)
      ((not (memf (lambda (p) (= (remainder n p) 0)) old-primes)) (iter (+ n 1) (cons n old-primes)))
      (else (iter (+ n 1) old-primes)))
    )
  (reverse (iter 2 empty))
  )

(primes 100)
