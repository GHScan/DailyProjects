#lang racket

(define (element-at l i)
  (if (= 1 i)
    (car l)
    (element-at (cdr l) (- i 1)))
  )
;------------------------------
(element-at '(a b) 2)
(element-at '(a b c) 2)
(element-at '(a b c d) 2)
