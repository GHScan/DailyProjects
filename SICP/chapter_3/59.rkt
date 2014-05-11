#lang racket

(provide (all-defined-out))

(define (stream-map proc . streams)
  (if (stream-empty? (car streams))
    empty-stream
    (stream-cons (apply proc (map stream-first streams))
                 (apply stream-map proc (map stream-rest streams)))
    )
  )

(define (integral-series s)
  (define (iter n s)
    (if (stream-empty? s)
      s
      (stream-cons (/ (stream-first s) n) (iter (+ n 1) (stream-rest s))))
    )
  (iter 1 s)
  )

(define (apply-series s x n)
 (define (iter s i)
   (if (or (stream-empty? s) (= i n))
     0
     (+ (stream-first s) (* x (iter (stream-rest s) (+ i 1)))))
   )
 (iter s 0)
 )

(define exp-series (stream-cons 1 (integral-series exp-series)))
(define sine-series (stream-cons 0 (integral-series cosine-series)))
(define cosine-series (stream-cons 1 (stream-map - (integral-series sine-series))))

(printf "exp^1 = ~a\n" (apply-series exp-series 1.0 30))
(printf "sin(0) = ~a\n" (apply-series sine-series 0 30))
(printf "sin(30) = ~a\n" (apply-series sine-series (/ pi 6) 30))
(printf "cos(0) = ~a\n" (apply-series cosine-series 0 30))
(printf "cos(60) = ~a\n" (apply-series cosine-series (/ pi 3) 30))
