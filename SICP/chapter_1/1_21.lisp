#lang racket
(require racket/pretty)

(define (smallest-divisor n)
  (define (smallest-divisor-iter n v)
    (cond 
      ((> (* v v) n)
       n)
      ((= 0 (remainder n v))
       v)
      (else 
        (smallest-divisor-iter n (+ v 1))))
    )

  (smallest-divisor-iter n 2)
  )

(pretty-print (smallest-divisor 199))
(pretty-print (smallest-divisor 1999))
(pretty-print (smallest-divisor 19999))
