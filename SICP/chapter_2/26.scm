#lang racket

(define x (list 1 2 3))
(define y (list 4 5 6))

(pretty-print (append x y))
(pretty-print (cons x y))
(pretty-print (list x y))
