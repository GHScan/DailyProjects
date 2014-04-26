#lang racket

(define (reverse this)
 (define (reverse-iter old new)
  (cond 
   ((null? old) new)
   (else (reverse-iter (cdr old) (cons (car old) new)))
   )
  )
 (reverse-iter this (list))
 )

(pretty-print (reverse (list 1)))
(pretty-print (reverse (list 1 2)))
(pretty-print (reverse (list 1 2 3)))
