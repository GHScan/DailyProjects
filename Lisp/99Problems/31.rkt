#lang racket

(define (is-prime n)
  (let iter ([i 2][max-i (floor (sqrt n))])
    (if (> i max-i)
      true
      (and (not (= 0 (remainder n i))) (iter (+ i 1) max-i))))
  )

;------------------------------
(filter is-prime (range 2 100))
