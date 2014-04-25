#lang racket
(require racket/pretty)

(define (float-equal? a b)
 (< (abs (- a b)) 0.001)
 )

(define (average a b) (/ (+ a b) 2))

(define (fixed-point f guess)
 (newline)
 (display guess)
 (let ((next-guess (f guess)))
  (cond 
   ((float-equal? guess next-guess) next-guess)
   (else (fixed-point f next-guess))
   ))
 )

(pretty-print (fixed-point (lambda (x) (/ (log 1000) (log x))) 2.0))
(pretty-print (fixed-point (lambda (x) (average x (/ (log 1000) (log x)))) 2.0))
