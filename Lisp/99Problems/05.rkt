#lang racket

(define (my-reverse l)
  (let iter ([l l][result empty])
    (if (empty? l)
      result
      (iter (cdr l) (cons (car l) result))))
  )

;------------------------------
(my-reverse '())
(my-reverse '(a))
(my-reverse '(a b))
(my-reverse '(a b c))
(my-reverse '(a b c d))
