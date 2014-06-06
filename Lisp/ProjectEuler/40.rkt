#lang racket

(require "utils.rkt")

(define (width-m-count m)
  (- (expt 10 m) (expt 10 (- m 1)))
  )

(define (kth-width-m-number k m)
  (+ k (expt 10 (- m 1)))
  )

(define (digit-of-number n i)
  (list-ref (number->list n) i)
  )

(define (d i)
  (let iter ([i i][w 1][count (width-m-count 1)])
    (if (< i (* w count))
      (digit-of-number (kth-width-m-number (quotient i w) w) (remainder i w))
      (iter (- i (* w count)) (+ w 1) (width-m-count (+ w 1)))))
  )

(apply * (map (lambda (i) (d (- (expt 10 i) 1))) (range 0 7)))
