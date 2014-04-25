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

(define (average-3 x y z)
  (/ (+ x y z) 3)
  )

(define dx 0.0001)

(define (smooth f)
  (lambda (x) (average-3 (f (- x dx)) (f x) (f (+ x dx))))
  )

(define (smooth-n f n)
 ((repeat smooth n) f)
 )

(define (for-each-print f next begin end)
  (cond
    ((< begin end) 
     (newline)
     (display (f begin))
     (for-each-print f next (next begin) end))
    )
  )

(define floor-smooth-4 (smooth-n floor 4))

(for-each-print floor-smooth-4 (lambda (x) (+ x 1)) 0 10)
