#lang racket

(define (compress l)
 (if (empty? l)
  empty
  (let iter ([v (car l)][l (cdr l)])
   (cond
    [(empty? l) (list v)]
    [(not (eq? v (car l))) (cons v (iter (car l) (cdr l)))]
    [else (iter v (cdr l))])))
 )
;------------------------------
(compress '())
(compress '(a))
(compress '(a b))
(compress '(a a b))
(compress '(a b b))
(compress '(a a b b))
(compress '(a a a a b c c a a d e e e e))
