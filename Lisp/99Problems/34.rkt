#lang racket

(provide totient-phi-1)
;------------------------------
(define (gcd a b)
  (if (= a 0)
    b
    (gcd (remainder b a) a))
  )

(define (coprime a b) 
  (= 1 (gcd a b))
  )

(define (totient-phi-1 m)
  (length (filter (curry coprime m) (range 1 m)))
  )
;------------------------------
(map (lambda (i) (list i (totient-phi-1 i))) (range 2 32))
