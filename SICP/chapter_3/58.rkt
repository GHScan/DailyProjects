#lang racket

(define (expand num den radix)
  (stream-cons (quotient (* num radix) den)
               (expand (remainder (* num radix) den) den radix))
  )

(define A (expand 1 7 10))
(define B (expand 3 8 10))

(build-list 16 (lambda (i) (stream-ref A i)))
(build-list 16 (lambda (i) (stream-ref B i)))
