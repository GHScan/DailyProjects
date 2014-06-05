#lang racket

(require "utils.rkt")

(define (number-of-divisor n)
  (foldl (lambda (v init) (* init (+ 1 (cdr v)))) 1 (find-prime-factors-pair n))
  )

(define sums (stream-cons 1 (stream-map + (in-naturals 2) sums)))
(define sums-divisor-number (stream-map (lambda (n) (list n (number-of-divisor n))) sums))
(stream-first (stream-filter (lambda (pair) (> (cadr pair) 500)) sums-divisor-number))
