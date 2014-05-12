#lang racket

(provide (all-defined-out))

(define (stream-map proc . streams)
  (if (stream-empty? (car streams))
    empty-stream
    (stream-cons (apply proc (map stream-first streams))
                 (apply stream-map proc (map stream-rest streams)))
    )
  )

(define (sqrt-stream x)
  (define s (stream-cons 1 (stream-map (lambda (v) (/ (+ v (/ x v)) 2)) s)))
  s)

(define (sqrt-stream2 x)
  (stream-cons 1 (stream-map (lambda (v) (/ (+ v (/ x v)) 2)) (sqrt-stream2 x)))
 )

(define (stream-take n s)
  (build-list n (lambda (i) (stream-ref s i)))
 )

;
(define sqrt-2 (sqrt-stream 2.0))
(define sqrt2-2 (sqrt-stream2 2.0))

(stream-take 10 sqrt-2)
;(stream-take 10 sqrt2-2)

;(length (stream-take 2000 sqrt-2))
;(length (stream-take 2000 sqrt2-2))
