#lang racket

(provide (all-defined-out))

(require "59.rkt")

(define (add-series s1 s2)
  (stream-map + s1 s2)
  )

(define (mul-series s1 s2)
  (stream-cons (* (stream-first s1) (stream-first s2)) 
               (add-series (mul-series (stream-rest s1) s2) 
                           (stream-map (lambda (x) (* x (stream-first s1))) (stream-rest s2))))
  )

(define sine^2+cosine^2 (add-series 
                          (mul-series sine-series sine-series)
                          (mul-series cosine-series cosine-series)))

(printf "sin(30)^2+cos(30)^2 = ~a\n" (apply-series sine^2+cosine^2 (/ pi 6) 30))
(printf "sin(60)^2+cos(60)^2 = ~a\n" (apply-series sine^2+cosine^2 (/ pi 3) 30))
