#lang racket

(require "66.rkt")

(define (pairs2 s0 s1)
  ; after removing (stream-cons s0-car s1-car), the recusion procedure pairs2 will be infinity
  (interleave (stream-map (lambda (x) (list (stream-first s0) x)) s1)
              (pairs2 (stream-rest s0) (stream-rest s1))) 
  )

(define int-pairs2 (pairs2 integers integers))

(build-list 0 (lambda (i) (stream-ref int-pairs2 i)))
