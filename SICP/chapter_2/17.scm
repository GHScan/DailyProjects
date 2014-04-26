#lang racket

(define (last-pair this)
  (cond 
    ((null? (cdr this)) (car this))
    (else (last-pair (cdr this))))
  )
    
(pretty-print (last-pair (list 1 2 3)))
(pretty-print (last-pair (list 1 2)))
(pretty-print (last-pair (list 1)))
