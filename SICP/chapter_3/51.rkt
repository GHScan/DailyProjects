#lang racket

(define (show x)
  (newline)
  (display x)
  x)

(define a (stream-map show (in-range 0 10)))

; Only output 5
; It means, both first and rest part of stream-cons in racket is lazy evaluation,
; it's different with SICP
(stream-ref a 5)
(stream-ref a 7)
