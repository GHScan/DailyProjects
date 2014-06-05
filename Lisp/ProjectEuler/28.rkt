#lang racket

(define (sum-border n)
  (if (= 1 n)
    1
    (- (+ (* 4 (* n n)) 6) (* 6 n)))
  )

(foldl + 0 (map sum-border (filter odd? (range 1 1002))))
