#lang racket

(define (union-set set1 set2)
  (cond 
    ((or (null? set1) (null? set2)) (append set1 set2))
    ((< (car set1) (car set2)) (cons (car set1) (union-set (cdr set1) set2)))
    ((> (car set1) (car set2)) (cons (car set2) (union-set set1 (cdr set2))))
    (else (cons (car set1) (union-set (cdr set1) (cdr set2))))
    )
  )
(define (intersection-set set1 set2)
  (cond 
    ((or (null? set1) (null? set2)) empty)
    ((< (car set1) (car set2)) (intersection-set (cdr set1) set2))
    ((> (car set1) (car set2)) (intersection-set set1 (cdr set2)))
    (else (cons (car set1) (intersection-set (cdr set1) (cdr set2))))
    )
  )
(define (element-of-set? x set)
  (cond
    ((null? set) false)
    ((= x (car set)) true)
    ((< x (car set)) false)
    (else (element-of-set? x (cdr set))))
  )
(define (adjoin-set x set)
  (cond
    ((null? set) (list x))
    ((= x (car set)) set)
    ((< x (car set)) (cons x set))
    (else (adjoin-set x (cdr set))))
  )

(define (list->set l)
  (foldr adjoin-set empty  l)
  )

(define set1 (list->set '(1 2 3 4 4 4 5)))
(define set2 (list->set '(2 2 4 6)))
(pretty-print set1)
(pretty-print set2)
(pretty-print (intersection-set set1 set2))
(pretty-print (union-set set1 set2))
