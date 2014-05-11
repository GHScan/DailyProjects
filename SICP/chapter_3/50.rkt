#lang racket

(define (stream-map proc . streams)
  (if (stream-empty? (car streams))
    empty-stream
    (stream-cons (apply proc (map stream-first streams))
                 (apply stream-map proc (map stream-rest streams))))
  )

(define a (stream-map + (in-range 10 20) (in-range 10)))
(stream-for-each pretty-print a)
