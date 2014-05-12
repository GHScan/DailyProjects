#lang racket

(provide (all-defined-out))

(define (stream-map proc . streams)
  (if (stream-empty? (car streams))
    empty-stream
    (stream-cons (apply proc (map stream-first streams))
                 (apply stream-map proc (map stream-rest streams)))
    )
  )

(define (integral lazy-stream init dt)
  (define (next-init s init)
    (+ init (* dt (stream-first s)))
    )
  (define (iter s init)
   (stream-cons init (iter (stream-rest s) (next-init s init)))
    )
  (stream-cons init
               (let ((s (stream-first lazy-stream)))
                 (iter (stream-rest s) (next-init s init))))
  )

(define (solve f y0 dt)
  (define y (integral (stream dy) y0 dt))
  (define dy (stream-map f y))
  y)

(stream-ref (solve identity 1 0.001) 1000)
