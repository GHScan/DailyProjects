#lang racket

(define (stream-map proc . streams)
  (if (stream-empty? (car streams))
    empty-stream
    (stream-cons (apply proc (map stream-first streams))
                 (apply stream-map proc (map stream-rest streams)))
    )
  )

(define factorials (stream-cons 1 (stream-map * factorials (in-naturals 2))))

(build-list 10 (lambda (i) (stream-ref factorials i)))
