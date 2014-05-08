#lang racket

(define (append a b) 
  (if (null? a) 
    b
    (mcons (mcar a) (append (mcdr a) b)))
  )

(define (append! a b)
  (set-mcdr! (last-pair a) b)
  a)

(define (last-pair a)
  (if (null? (mcdr a))
    a
    (last-pair (mcdr a)))
  )

(define (puts x)
  (display x)
  (newline)
  )

(define x (mcons 'a (mcons 'b empty)))
(define y (mcons 'c (mcons 'd empty)))
(define z (append x y))

(puts z)
(puts (mcdr x))

(define w (append! x y))
(puts w)
(puts (mcdr x))
