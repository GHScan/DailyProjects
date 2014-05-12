#lang racket

(require "77.rkt")

(define (solve-2nd f dt y0 dy0)
 (define y (integral (stream dy) y0 dt))
 (define dy (integral (stream ddy) dy0 dt))
 (define ddy (stream-map f dy y))
 y)

(stream-ref (solve-2nd (lambda (dy y) dy) 0.001 1 1) 1000)

(define sqr-x+1 (solve-2nd (lambda (dy y) (+ (sqr dy) (- (* y 4)) 2)) 0.1 1 2))
(stream-ref sqr-x+1 10)
