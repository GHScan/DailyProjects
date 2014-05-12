#lang racket

(require "66.rkt")

(define (pairs-all s0 s1)
  (stream-cons (list (stream-first s0) (stream-first s1))
               (interleave (stream-map (lambda (x) (list (stream-first s0) x)) (stream-rest s1))
                           (interleave 
                             (stream-map (lambda (x) (list x (stream-first s1))) (stream-rest s0))
                             (pairs-all (stream-rest s0) (stream-rest s1)))))
  )

(define int-all-pairs (pairs-all integers integers))

(build-list 32 (lambda (i) (stream-ref int-all-pairs i)))
