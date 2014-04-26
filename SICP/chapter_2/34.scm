#lang racket
(require racket/pretty)

(define (accumulate f init l)
  (cond 
    ((null? l) init)
    (else (f (car l) (accumulate f init (cdr l)))))
  )

(define (horner-eval x l) 
 (accumulate (lambda (i init) (+ i (* init x))) 0 l)
 )

(pretty-print (horner-eval 2 (list 1 3 0 5 0 1)))
(pretty-print (horner-eval 4 (list 1 2 1)))
