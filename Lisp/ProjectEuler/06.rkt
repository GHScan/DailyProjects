#lang racket

(define (solve n)
  (let ([s (in-range 1 (+ n 1))])
    (- (sqr (stream-fold + 0 s))
       (stream-fold + 0 (stream-map sqr s))))
  )

(solve 10)
(solve 100)
