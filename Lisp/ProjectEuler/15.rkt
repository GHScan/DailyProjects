#lang racket

(require "utils.rkt")

(define (solve m n)
  (cond
    [(zero? m) 1]
    [(zero? n) 1]
    [else (+ (solve (- m 1) n) (solve m (- n 1)))])
  )
(set! solve (memoize solve))

(solve 20 20)
