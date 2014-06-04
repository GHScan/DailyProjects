#lang racket

(define (prime-factors n)
  (let iter ([i 2][n n])
    (cond
      [(= n 1) empty]
      [(> (* i i) n) (list n)]
      [(= 0 (remainder n i)) (cons i (iter i (quotient n i)))]
      [else (iter (+ i 1) n)]))
  )
;------------------------------
(map prime-factors (range 1 20))
(prime-factors 315)
