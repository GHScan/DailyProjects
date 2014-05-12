#lang racket

(require "63.rkt")

(define (stream-limit s tolerance)
  (let ((x (stream-ref s 0))(y (stream-ref s 1)))
    (if (< (abs (- x y)) tolerance)
      y
      (stream-limit (stream-rest s) tolerance)))
  )

(define (sqrt x tolerance)
  (stream-limit (sqrt-stream x) tolerance)
  )

(sqrt 2.0 0.0001)
(sqrt 3.0 0.0001)
(sqrt 4.0 0.0001)
(sqrt 5.0 0.0001)
