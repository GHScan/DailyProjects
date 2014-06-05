#lang racket

(define (sum-of-number-digits n)
  (let iter ([n n][result 0])
    (if (zero? n)
      result
      (iter (quotient n 10) (+ (remainder n 10) result))))
  )

;(sum-of-number-digits (expt 2 15))
(sum-of-number-digits (expt 2 1000))
