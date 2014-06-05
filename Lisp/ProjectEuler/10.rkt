#lang racket

(require "utils.rkt")

(define (sum-primes max)
  (stream-fold + 0 (stream-take-until prime-list (lambda (n) (>= n max))))
  )

(sum-primes 10)
(sum-primes 2000000)
