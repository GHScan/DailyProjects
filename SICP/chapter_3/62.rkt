#lang racket

(require "59.rkt")
(require "60.rkt")

(define (reciprocals S)
  (if (= 1 (stream-first S))
    'ok
    (error "the constant of S must be 1"))

  (define X (stream-cons 1 (mul-series X (stream-map - (stream-rest S)))))
  X)

(define (div-series s1 s2)
  (mul-series s1 (reciprocals s2))
  )

(define tan-series (div-series sine-series cosine-series))
(printf "tan(45) = ~a\n" (apply-series tan-series (/ pi 4) 30))
