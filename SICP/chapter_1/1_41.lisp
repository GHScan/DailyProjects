#lang racket
(require racket/pretty)

(define (double f)
 (lambda (x) (f (f x)))
 )

(define (inc x)
 (+ 1 x)
 )

(pretty-print (((double (double double)) inc) 5))
