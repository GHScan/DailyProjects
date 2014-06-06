#lang racket

(require "utils.rkt")

(define triangles (stream-map (lambda (n) (/ (* n (+ n 1)) 2)) (in-naturals 1)))
(define pentagonals (stream-map (lambda (n) (/ (* n (- (* 3 n) 1)) 2)) (in-naturals 1)))
(define hexagonal (stream-map (lambda (n) (* n (- (* 2 n) 1))) (in-naturals 1)))

(stream->list (stream-take (stream-intersect identity triangles pentagonals hexagonal) 3))
