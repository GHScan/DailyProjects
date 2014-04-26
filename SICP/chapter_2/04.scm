#lang racket

(define (cons x y)
  (lambda (b) (if b x y))
  )
(define (car p)
  (p true)
  )
(define (cdr p)
  (p false)
  )

(define (cons2 x y)
  (lambda (s) (s x y))
  )
(define (car2 p)
  (p (lambda (x y) x))
  )
(define (cdr2 p)
  (p (lambda (x y) y))
  )

(define seq (cons 1 (cons 2 3)))
(pretty-print (car seq))
(pretty-print (car (cdr seq)))

(define seq2 (cons2 1 (cons2 2 3)))
(pretty-print (car2 seq2))
(pretty-print (car2 (cdr2 seq2)))
