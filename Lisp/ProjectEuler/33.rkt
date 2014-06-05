#lang racket

(require "utils.rkt")

(apply *
       (list-comprehension
         (/ x y)
         (x <- (range 10 100))
         (y <- (range (+ x 1) 100))
         (= (remainder x 10) (quotient y 10))
         (not (zero? y))
         (not (zero? (remainder y 10)))
         (= (/ x y) (/ (quotient x 10) (remainder y 10)))
         ))
