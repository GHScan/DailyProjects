#lang racket

(require "utils.rkt")

(list-comprehension
  (* a b c)
  (a <- (range 1 1000))
  (b <- (range a 1000))
  (c <- (list (sqrt (+ (sqr a) (sqr b)))))
  (< c 1000)
  (integer? c)
  (= 1000 (+ a b c))
  )

