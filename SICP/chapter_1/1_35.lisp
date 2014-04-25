#lang racket
(require racket/pretty)

(define (float-equal? a b)
 (< (abs (- a b)) 0.001)
 )

(define (fixed-point f guess)
 (let ((next-guess (f guess)))
  (cond 
   ((float-equal? guess next-guess) next-guess)
   (else (fixed-point f next-guess))
   ))
 )

(pretty-print (fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0))
