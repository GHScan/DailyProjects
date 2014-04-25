#lang racket
(require racket/pretty)

(define dx 0.0001)

(define (float-equal? x y)
  (< (abs (- x y)) dx)
  )

(define (deriv f) 
  (lambda (x) (/ (- (f (+ x dx)) (f x)) dx))
  )

(define (newton-transform g)
  (lambda (x) (- x (/ (g x) ((deriv g) x))))
  )

(define (fixed-point f guess)
  (let ((next-guess (f guess)))
    (cond 
      ((float-equal? guess next-guess) next-guess)
      (else (fixed-point f next-guess))))
  )

(define (newton-method f guess)
  (fixed-point (newton-transform f) guess)
  )

(define (sqrt x)
 (newton-method (lambda (y) (- x (* y y))) 1.0)
 )

(define (cubic a b c)
 (lambda (x) (+ (* x x x) (* a x x) (* b x) c))
 )

(pretty-print (sqrt 2))
(pretty-print (sqrt 3))
(pretty-print (newton-method (cubic 1 1 1) 1.0))
