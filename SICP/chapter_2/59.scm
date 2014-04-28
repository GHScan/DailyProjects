#lang racket

(define (union-set set1 set2)
  (cond 
    ((or (null? set1) (null? set2)) 
     (append set1 set2))
    ((element-of-set? (car set1) set2) 
     (union-set (cdr set1) set2))
    (else
      (cons (car set1) (union-set (cdr set1) set2)))
    )
  )
(define (intersection-set set1 set2)
  (cond
    ((or (null? set1) (null? set2)) 
     empty)
    ((element-of-set? (car set1) set2) 
     (cons (car set1) (intersection-set (cdr set1) set2)))
    (else 
      (intersection-set (cdr set1) set2))
    )
  )
(define (element-of-set? x set)
  (cond 
    ((null? set) false)
    ((= x (car set)) true)
    (else (element-of-set? x (cdr set))))
  )
(define (adjoin-set x set)
  (if (element-of-set? x set)
    set
    (cons x set))
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
