#lang racket

(define (gcd a b)
  (if (= a 0)
    b
    (gcd (remainder b a) a))
  )
;------------------------------
(gcd 2 8)
(gcd 3 8)
(gcd 4 8)
(gcd 5 8)
(gcd 4 12)
(gcd 5 12)
(gcd 6 12)
(gcd 36 63)
