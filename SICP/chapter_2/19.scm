#lang racket

(define (cc amount coins) 
  (cond
    ((< amount 0) 0)
    ((= amount 0) 1)
    ((null? coins) 0)
    (else (+ (cc amount (cdr coins))
             (cc (- amount (car coins)) coins))))
  )

(pretty-print (cc 100 (list 50 25 10 5 1)))
(pretty-print (cc 100 (list 100 50 20 10 5 2 1 0.5)))
