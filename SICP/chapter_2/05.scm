#lang racket

(define (fetch-pow-k v k)
  (define (fetch-pow-k-iter v n)
    (cond
      ((> (remainder v k) 0) n)
      (else (fetch-pow-k-iter (quotient v k) (+ n 1))))
    )
  (fetch-pow-k-iter v 0)
  )

(define (square x)
 (* x x)
 )

(define (pow x y)
  (cond 
    ((= y 0) 1)
    ((even? y) (square (pow x (quotient y 2))))
    (else (* x (pow x (- y 1)))))
  )

(define (cons x y)
 (* (pow 2 x) (pow 3 y))
  )
(define (car p)
  (fetch-pow-k p 2)
  )
(define (cdr p)
  (fetch-pow-k p 3)
  )

(pretty-print (car (cons 0 0)))
(pretty-print (cdr (cons 0 0)))
(pretty-print (car (cons 0 1)))
(pretty-print (cdr (cons 0 1)))
(pretty-print (car (cons 1 2)))
(pretty-print (cdr (cons 1 2)))
(pretty-print (car (cons 2 3)))
(pretty-print (cdr (cons 2 3)))
(pretty-print (car (cons 123 456)))
(pretty-print (cdr (cons 123 456)))
