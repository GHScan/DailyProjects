#lang racket

(define (lcm a b)
  (/ (* a b) (gcd a b))
  )

;(foldl lcm 1 (range 1 11))
(foldl lcm 1 (range 1 21))
