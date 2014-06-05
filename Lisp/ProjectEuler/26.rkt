#lang racket

(require "utils.rkt")

(define (index-of l v)
  (if (equal? v (car l)) 
    0
    (+ 1 (index-of (cdr l) v)))
  )

(define (repeat-length n m)
  (let iter ([n n][history empty])
    (cond
      [(zero? n) 0]
      [(member n history) (+ 1 (index-of history n))]
      [else (iter (* 10 (remainder n m)) (cons n history))]))
  )

(define num-repeat (map (lambda (i) (list i (repeat-length 1 i))) (range 1 1000)))
(define max-repeat (apply max (map cadr num-repeat)))
(car (memf (lambda (pair) (= max-repeat (cadr pair))) num-repeat))
