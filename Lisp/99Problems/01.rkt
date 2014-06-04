#lang racket

(define (my-last l)
  (if (empty? (cdr l))
    (car l)
    (my-last (cdr l)))
  )
;------------------------------
(my-last '(a))
(my-last '(a b))
(my-last '(a b c))
(my-last '(a b c d))
