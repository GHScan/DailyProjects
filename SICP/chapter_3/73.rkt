#lang racket

(define (stream-map proc . streams)
  (if (stream-empty? (car streams))
    empty-stream
    (stream-cons (apply proc (map stream-first streams))
                 (apply stream-map proc (map stream-rest streams)))
    )
  )

(define (partial-sums s)
  (define news (stream-cons (stream-first s) (stream-map + news (stream-rest s))))
  news)

(define (RC R C dt)
  (lambda (i v0)
    (stream-map (curry + v0) (stream-map (curry * R) i) (stream-map (curry * (/ dt C)) (partial-sums i)))))

(define dt 0.1)
(define RC1 (RC 5 1 dt))
(define i (stream-map sin (stream-map (curry * pi dt 2) (in-naturals))))
(define v (RC1 i 0))

(build-list 20 (curry stream-ref i))
(build-list 20 (curry stream-ref v))
