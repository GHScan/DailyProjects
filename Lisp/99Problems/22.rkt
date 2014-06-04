#lang racket

(define (range a b)
  (cond 
    [(= a b) (list a)]
    [(< b a) (range b a)]
    [else (cons a (range (+ a 1) b))])
  )
;------------------------------
(range 4 9)
(range 4 4)
(range 4 1)
