#lang racket

(define (square x)
  (* x x)
  )

(define (square-list items)
  (cond
    ((null? items) items)
    (else (cons (square (car items)) (square-list (cdr items))))
    )
  )
(define (square-list2 items)
 (map square items)
  )

(pretty-print (square-list (list 1 2 3 4 5)))
(pretty-print (square-list2 (list 1 2 3 4 5)))
