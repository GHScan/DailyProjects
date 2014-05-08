#lang racket

(define (make-accumulator value)
  (lambda (inc) 
    (set! value (+ inc value))
    value
    ))

(define a1 (make-accumulator 10))
(define a2 (make-accumulator 20))
(define a3 (make-accumulator 30))
(a1 5)
(a2 10)
(a3 15)
(a1 15)
