#lang racket

(require "77.rkt")

(define (solve-2nd a b dt y0 dy0)
 (define y (integral (stream dy) y0 dt))
 (define dy (integral (stream ddy) dy0 dt))
 (define ddy (stream-map + (stream-map (curry * dt a) dy) (stream-map (curry * dt dt b) y)))
 y)

(stream-ref (solve-2nd 0.5 0.5 0.001 1 1) 1000)
