#lang racket

(provide (all-defined-out))
;------------------------------
(define (list->set l)
  (cond
    [(empty? l) empty]
    [(member (car l) (cdr l)) (list->set (cdr l))]
    [else (cons (car l) (list->set (cdr l)))])
  )
(define (set-union l1 l2)
  (cond
    [(empty? l1) l2]
    [(member (car l1) l2) (set-union (cdr l1) l2)]
    [else (cons (car l1) (set-union (cdr l1) l2))])
  )
(define (set-intersection l1 l2)
  (cond
    [(empty? l1) empty]
    [(member (car l1) l2) (cons (car l1) (set-intersection (cdr l1) l2))]
    [else (set-intersection (cdr l1) l2)])
  )
(define (set-difference l1 l2)
  (cond
    [(empty? l1) empty]
    [(member (car l1) l2) (set-difference (cdr l1) l2)]
    [else (cons (car l1) (set-difference (cdr l1) l2))])
  )
