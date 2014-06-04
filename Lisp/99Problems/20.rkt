#lang racket

(define (remove-at l i)
  (if (= 1 i)
    (cdr l)
    (cons (car l) (remove-at (cdr l) (- i 1))))
  )
;------------------------------
(remove-at '(a b) 2)
(remove-at '(a b c) 2)
(remove-at '(a b c d) 2)
