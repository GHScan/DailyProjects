#lang racket

(define (my-but-last l)
  (if (empty? (cddr l))
    l
    (my-but-last (cdr l)))
  )
;------------------------------
(my-but-last '(a b))
(my-but-last '(a b c))
(my-but-last '(a b c d))
