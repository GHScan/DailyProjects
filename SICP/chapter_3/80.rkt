#lang racket

(require "77.rkt")

(define (RLC R L C dt)
  (lambda (vc0 il0)
    (define vc (integral (stream (stream-map (curry * (/ -1 C)) il)) vc0 dt))
    (define il (integral 
                 (stream (stream-map + 
                                     (stream-map (curry * (/ 1 L)) vc) 
                                     (stream-map (curry * (- (/ R L))) il))) 
                 il0 dt))
    (stream-map cons vc il))
  )

(define RLC1 (RLC 1 1 0.2 0.1))
(define RLC1-r (RLC1 10 0))

(build-list 20 (curry stream-ref RLC1-r))
