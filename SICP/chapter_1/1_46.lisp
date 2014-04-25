#lang racket
(require racket/pretty)

(define (iterative-improve good-enough? improve)
  (define (iter x)
   (cond 
    ((good-enough? x) x)
    (else (iter (improve x))))
   )
  iter
 )

(define (float-equal? x y)
 (< (abs (- x y)) 0.001)
 )

(define (average x y)
 (/ (+ x y) 2)
 )

(define (sqrt x)
 (define (good-enough? y) (float-equal? (* y y) x))
 (define (improve y) (average y (/ x y)))
 ((iterative-improve good-enough? improve) 1.0)
 )

(define (fixed-point f guess)
 (define (good-enough? x) (float-equal? (f x) x))
 (define (improve x) (f x))
 ((iterative-improve good-enough? improve) guess)
 )

(define (sqrt-2 x)
 (fixed-point (lambda (y) (average y (/ x y))) 1.0)
 )

(pretty-print (sqrt 2))
(pretty-print (sqrt 3))
(pretty-print (sqrt 4))
(pretty-print (sqrt 9))
(pretty-print (fixed-point cos 1))
(pretty-print (fixed-point sin 1))
(pretty-print (sqrt-2 2))
(pretty-print (sqrt-2 3))
(pretty-print (sqrt-2 4))
(pretty-print (sqrt-2 9))
