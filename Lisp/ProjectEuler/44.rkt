#lang racket

(require "utils.rkt")

(define (pentagon-number? p)
  (and (> p 0) (integer? (/ (+ 1 (sqrt (+ 1 (* 24 p)))) `6)))
  )

(define (Pn n)
  (/ (* n (- (* 3 n) 1)) 2)
  )

(list-comprehension
  (list d (Pn d))
  (j <- (range 1 2000))
  (d <- (range 1 2000))
  (let* ([pj (Pn j)][pd (Pn d)][pk (+ pj pd)][sum (+ pj pk)])
    (and (pentagon-number? pk) (pentagon-number? sum)))
  )

