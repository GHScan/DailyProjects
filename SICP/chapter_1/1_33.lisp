#lang racket
(require racket/pretty)

(define (reduce op map filter begin end init)
  (cond
    ((>= begin end)
     init)
    ((filter begin)
     (reduce op map filter (+ begin 1) end (op init (map begin))))
    (else 
      (reduce op map filter (+ begin 1) end init))
    )
  )

(define (prime? n)
 (define (test-divisor n a)
  (cond 
   ((> (* a a) n) n)
   ((= (remainder n a) 0) a)
   (else (test-divisor n (+ a 1))))
  )
 (and 
  (= (test-divisor n 2) n)
  (> n 1))
 )

(define (gcd a b)
 (cond 
  ((= b 0) a)
  (else (gcd b (remainder a b))))
 )

(define (sum-primes a b) (reduce + identity prime? a b 0))
(define (sum-prime-with-n n) 
 (define (fit i) (= 1 (gcd n i)))
 (reduce * identity fit 1 (- n 1) 1))

(pretty-print (sum-primes 1 100))
(pretty-print (sum-prime-with-n 15))
