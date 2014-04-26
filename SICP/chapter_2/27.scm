#lang racket

(define (deep-reverse this)
  (define (deep-reverse-iter oldl newl)
    (cond
      ((null? oldl) newl)
      (else 
        (let ((n (car oldl)))
          (let ((revn (if (pair? n) (deep-reverse n) n)))
            (deep-reverse-iter (cdr oldl) (cons revn newl)))))
      )
    )
  (deep-reverse-iter this (list))
  )

(define x (list (list 1 2) (list 3 4)))
(pretty-print x)
(pretty-print (reverse x))
(pretty-print (deep-reverse x))
