#lang racket

(define (insert-at v l i)
  (if (= 1 i)
    (cons v l)
    (cons (car l) (insert-at v (cdr l) (- i 1))))
  )
;------------------------------
(insert-at 123 '(a b) 2)
(insert-at 123 '(a b c) 2)
(insert-at 123 '(a b c d) 2)
