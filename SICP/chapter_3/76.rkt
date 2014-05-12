#lang racket

(require "74.rkt")

(define (smooth s last-value)
  (if (stream-empty? s)
    empty-stream
    (stream-cons (/ (+ (stream-first s) last-value) 2)
                 (smooth (stream-rest s) (stream-first s))))
  )

(define noise-data (stream 1 2 1.5 1 0.5 -0.1 -2 -3 0.02 -2 -0.5 0.2 3 4))
(define noise-zero-crossing (make-zero-crossings noise-data 0))
(define smoothed-noise-zero-crossing (make-zero-crossings (smooth noise-data 0) 0))

(stream->list noise-zero-crossing)
(stream->list smoothed-noise-zero-crossing)
