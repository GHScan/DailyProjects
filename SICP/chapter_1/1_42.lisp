#lang racket
(require racket/pretty)

(define (compose f g)
  (lambda (x) (f (g x)))
  )

(define (inc x)
  (+ 1 x)
  )

(define (square x)
  (* x x)
  )

(pretty-print ((compose square inc) 6))
