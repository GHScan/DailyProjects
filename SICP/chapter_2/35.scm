#lang racket
(require racket/pretty)

(define (accumulate f init l)
  (cond 
    ((null? l) init)
    (else (f (car l) (accumulate f init (cdr l)))))
  )

(define (count-leaves tree)
 (accumulate (lambda (subtree init) (+ init (if (pair? subtree) (count-leaves subtree) 1))) 0 tree)
 )

(pretty-print (count-leaves (list 1 2 (list 3 4) (list 5) 6 (list 7 (list 8 (list 9))))))
(pretty-print (count-leaves (list (list 0) (list 1 2) (list 3))))
