#lang racket

(require "utils.rkt")

(define (can-combine? n)
  (do ([prime-list prime-list (stream-rest prime-list)])
    ((or (> (stream-first prime-list) n) (integer? (sqrt (/ (- n (stream-first prime-list)) 2)))) 
     (< (stream-first prime-list) n))
    )
  )

(define odd-composite (stream-filter (lambda (n) (and (odd? n) (not (prime? n)))) (in-naturals 2)))
(define combinable (stream-filter can-combine? (in-naturals 2)))
(stream->list
 (stream-take 
  (stream-subtract identity odd-composite combinable)
  1))
