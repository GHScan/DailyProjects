#lang racket

(define (stream-map proc . streams)
  (if (stream-empty? (car streams))
    empty-stream
    (stream-cons (apply proc (map stream-first streams))
                 (apply stream-map proc (map stream-rest streams)))
    )
  )

(define ones (stream-cons 1 ones))
(define integers (stream-cons 1 (stream-map + ones integers)))

(define (partial-sums s)
  (define news (stream-cons (stream-first s) (stream-map + (stream-rest s) news)))
  news)

(define partial-sums-integers (partial-sums integers))

(build-list 10 (lambda (i) (stream-ref partial-sums-integers i)))
