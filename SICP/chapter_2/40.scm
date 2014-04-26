#lang racket
(require racket/pretty)

(define (prime? n)
  (define (find-divisor test)
    (cond 
      ((> (* test test) n) n)
      ((= 0 (remainder n test)) test)
      (else (find-divisor (+ test 1))))
    )
  (and 
    (= n (find-divisor 2))
    (> n 1))
  )

(define (range first last)
  (cond 
    ((> first last) (list))
    (else (cons first (range (+ first 1) last))))
  )

(define (flatmap f l)
  (foldr append (list) (map f l))
  )

(define (unique-pairs n)
  (flatmap
    (lambda (i) (map 
                  (lambda (j) (cons i j))
                  (range 1 (- i 1))))
    (range 1 n))
  )

(define (prime-sum-pairs n)
  (filter (lambda (x) (prime? (+ (car x) (cdr x)))) (unique-pairs n))
  )

(pretty-print (prime-sum-pairs 6))
