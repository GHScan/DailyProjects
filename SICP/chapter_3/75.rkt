#lang racket

(require "74.rkt")

(define (make-smooth-zero-crossing input-stream last-value last-avg)
  (if (stream-empty? input-stream)
    empty-stream
    (let ((avg (/ (+ last-value (stream-first input-stream)) 2)))
      (stream-cons (sign-change-detector last-avg avg) 
                   (make-smooth-zero-crossing (stream-rest input-stream) (stream-first input-stream) avg))))
  )

(define noise-data (stream 1 2 1.5 1 0.5 -0.1 -2 -3 0.02 -2 -0.5 0.2 3 4))
(define noise-zero-crossing (make-zero-crossings noise-data 0))
(define smoothed-noise-zero-crossing (make-smooth-zero-crossing noise-data 0 0))

(stream->list noise-zero-crossing)
(stream->list smoothed-noise-zero-crossing)
