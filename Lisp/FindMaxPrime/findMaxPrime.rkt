#lang racket

(define (choose-p n)
  (let ((p (random n)))
    (if (= 0 p)
      (choose-p n)
      p))
  )

(define (pow-mod a b m)
  (cond
    ((zero? b) 1)
    ((even? b) (remainder (sqr (pow-mod a (quotient b 2) m)) m))
    (else (remainder (* a (pow-mod a (- b 1) m)) m)))
  )

(define (prime? n)
  (andmap (lambda (_) (= 1 (pow-mod (choose-p n) (- n 1) n))) (range 10))
  )

(define (find-max-prime n)
  (if (prime? n)
    n
    (find-max-prime (sub1 n)))
  )

(for-each 
  (lambda (i) (let ((n (expt 2 i))) (printf "~a : ~a\n" n (find-max-prime n)))) 
  (range 1 32))
