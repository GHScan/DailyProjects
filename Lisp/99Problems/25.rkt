#lang racket

(define (element-at l i)
  (if (= 0 i)
    (car l)
    (element-at (cdr l) (- i 1)))
  )

(define (remove-at l i)
  (if (= 0 i)
    (cdr l)
    (cons (car l) (remove-at (cdr l) (- i 1))))
  )

(define (rnd-select l m)
  (let iter ([n (length l)][m m][l l])
    (if (= 0 m)
      empty
      (let ([i (random n)])
        (cons (element-at l i) (iter (- n 1) (- m 1) (remove-at l i))))))
  )

(define (rnd-perm l)
  (rnd-select l (length l))
  )
;------------------------------
(rnd-perm '(a b c))
(rnd-perm '(a b c d))
(rnd-perm '(a b c d e))
(rnd-perm '(a b c d e g h))
