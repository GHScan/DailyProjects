#lang racket
(require racket/pretty)

(define (fold-right f init l)
  (cond
    ((null? l) init)
    (else (f (car l) (fold-right f init (cdr l)))))
  )

(define (fold-left f init l)
  (cond
    ((null? l) init)
    (else (fold-left f (f init (car l)) (cdr l))))
  )

(pretty-print (fold-right / 1 (list 1 2 3)))
(pretty-print (fold-left / 1 (list 1 2 3)))
(pretty-print (fold-right list (list) (list 1 2 3)))
(pretty-print (fold-left list (list) (list 1 2 3)))
