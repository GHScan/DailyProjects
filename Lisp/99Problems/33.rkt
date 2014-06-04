#lang racket

(define (gcd a b)
  (if (= a 0)
    b
    (gcd (remainder b a) a))
  )

(define (coprime a b) 
  (= 1 (gcd a b))
  )
;------------------------------
(coprime 35 64)
