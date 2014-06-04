#lang racket

(define (split l n)
  (let iter ([n n][l l][left empty])
    (if (= 0 n)
      (list (reverse left) l)
      (iter (- n 1) (cdr l) (cons (car l) left))))
  )

(define (rotate l n)
  (if (< n 0)
    (rotate l (+ n (length l)))
    (let ([lr (split l n)])
      (append (cadr lr) (car lr))))
  )
;------------------------------
(rotate '(a b c d e f g h) 3)
(rotate '(a b c d e f g h) -2)
