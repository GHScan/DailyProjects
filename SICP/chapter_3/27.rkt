#lang racket

(require "26.rkt")

(display "----------------\n")

(define (memoize f)
  (define cache (make-table <))
  (lambda (x)
    (let ((value (cache 'lookup x)))
      (if value 
        value
        (begin (set! value (f x))
               (cache 'insert! x value)
               value))))
  )

;
(define memo-fib 
  (memoize 
    (lambda (x)
      (if (<= x 1)
        1
        (+ (memo-fib (- x 1)) (memo-fib (- x 2))))))
  )

(build-list 64 memo-fib)

;
(define (memo-fib2 x)
  (if (<= x 1)
    1
    (+ (memo-fib2 (- x 1)) (memo-fib2 (- x 2))))
  )
(set! memo-fib2 (memoize memo-fib2))

(build-list 64 memo-fib2)
