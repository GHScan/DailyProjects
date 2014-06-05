#lang racket

(require "utils.rkt")

(define (non-abundant? n)
  (< n (apply + (proper-divisors n)))
  )

(define non-abundants (filter non-abundant? (range 2 28123)))
(define numbers (make-vector 28124 false))

(do ([l1 non-abundants (cdr l1)])
 ((empty? l1))
 (do ([l2 non-abundants (cdr l2)][upper (- 28123 (car l1))])
  ((> (car l2) upper))
  (vector-set! numbers (+ (car l1) (car l2)) true)))

(apply + 
       (filter (lambda (i) (not (vector-ref numbers i))) (range 1 28123)))
