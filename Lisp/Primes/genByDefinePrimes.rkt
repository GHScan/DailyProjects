#lang racket

(define (stream-takef prec s)
  (cond
    ((stream-empty? s) s)
    ((prec (stream-first s)) (stream-cons (stream-first s) (stream-takef prec (stream-rest s))))
    (else empty-stream))
  )

(define (prime? n)
  (stream-andmap (lambda (p) (> (remainder n p) 0)) (stream-takef (lambda (p) (<= (sqr p) n)) primeList))
  )

(define primeList (stream-cons 2 (stream-filter prime? (in-naturals 3))))

(define (primes max-n)
  (stream->list (stream-takef (lambda (p) (< p max-n)) primeList))
  )

(primes 100)
