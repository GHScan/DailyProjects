#lang racket
(require racket/pretty)

(define (smallest-divisor n)
  (define (smallest-divisor-iter n v)
    (cond 
      ((> (* v v) n)
       n)
      ((= 0 (remainder n v))
       v)
      (else 
        (smallest-divisor-iter n (+ v 1))))
    )
  (smallest-divisor-iter n 2)
  )
(define (prime? n) (= (smallest-divisor n) n))

(define (search-for-primes start end)
  (define (report-number-time n elapse-time)
    (newline)
    (print n)
    (print " *** ")
    (print elapse-time)
    )
  (define (test-prime-and-print n start-time) 
    (cond 
      ((prime? n)
       (report-number-time n (- (current-inexact-milliseconds) start-time))
       ))
    )

  (test-prime-and-print start (current-inexact-milliseconds))

  (cond
    ((< start end) 
     (search-for-primes (+ start 1) end)
     ))
  )

(search-for-primes 10007 10037)
(search-for-primes 100003 100043)
(search-for-primes 1000003 1000037)
(search-for-primes 10000019 10000103)
