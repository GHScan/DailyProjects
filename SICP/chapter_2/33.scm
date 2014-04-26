#lang racket
(require racket/pretty)

(define (accumulate f init l)
  (cond 
    ((null? l) init)
    (else (f (car l) (accumulate f init (cdr l)))))
  )

(define (map f l)
 (accumulate (lambda (x init) (cons (f x) init)) (list) l)
 )
(define (append l1 l2)
 (accumulate cons l2 l1)
 )
(define (length l)
 (accumulate (lambda (a init) (+ 1 init)) 0 l)
 )

(define x (list 1 2 3 4 5))
(pretty-print (map (lambda (i) (* i i)) x))
(pretty-print (append x (list 6 7 8)))
(pretty-print (length x))
