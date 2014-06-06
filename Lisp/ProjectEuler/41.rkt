#lang racket

(require "utils.rkt")

(define N 7)
(define table (make-prime-table (expt 10 N)))

(do ([w 1 (+ 1 w)])
 ((> w N))
 (let ([nums (filter (lambda (n) (vector-ref table n)) (map list->number (partial-permuation (range 1 (+ w 1)) w)))])
  (if (empty? nums)
   'ok
   (pretty-print (apply max nums)))))
