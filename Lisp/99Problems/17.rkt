#lang racket

(define (split l n)
  (let iter ([n n][l l][left empty])
    (if (= 0 n)
      (list (reverse left) l)
      (iter (- n 1) (cdr l) (cons (car l) left))))
  )
;------------------------------
(split '(a b c) 3)
(split '(a b c d) 3)
(split '(a b c d e f g h i k) 3)
