#lang racket
(require racket/pretty)

(define (compose f g)
 (lambda (x) (f (g x)))
 )

(define (even? n)
 (= 0 (remainder n 2))
 )

(define (div x y)
  (floor (/ x y))
 )

(define (repeat f n)
 (cond 
  ((= 1 n) f)
  ((even? n) (repeat (compose f f) (div n 2)))
  (else (compose f (repeat f (- n 1)))))
 )

(define (inc x) 
  (+ x 1)
 )

(define (square x) 
  (* x x)
 )

(pretty-print ((repeat inc 16) 5))
(pretty-print ((repeat square 2) 5))
(pretty-print ((repeat square 4) 2))
