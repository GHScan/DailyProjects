#lang racket

(define (pow-mod a b m)
  (cond
    [(zero? b) 1]
    [(odd? b) (remainder (* a (pow-mod a (- b 1) m)) m)]
    [else (remainder (sqr (pow-mod a (/ b 2) m)) m)])
  )

(define mod-num (expt 10 10))
(define (solve n)
  (foldl (lambda (n init) (remainder (+ init (pow-mod n n mod-num)) mod-num)) 0 (range 1 (+ n 1)))
  )

(solve 1000)
