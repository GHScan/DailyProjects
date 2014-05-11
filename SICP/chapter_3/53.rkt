#lang racket

(define (stream-map proc . streams)
  (if (stream-empty? (car streams))
    empty-stream
    (stream-cons (apply proc (map stream-first streams))
                 (apply stream-map proc (map stream-rest streams)))
    )
  )

(define s (stream-cons 1 (stream-map + s s)))

(build-list 10 (lambda (i) (stream-ref s i)))
