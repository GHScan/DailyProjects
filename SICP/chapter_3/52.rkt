#lang racket

(define sum 0)
(define (accum x) 
 (set! sum (+ sum x))
 sum)

(define seq (stream-map accum (in-range 1 20)))
(define y (stream-filter even? seq))
(define z (stream-filter (lambda (x) (= (remainder x 5) 0)) seq))

(stream-ref y 7)

(printf "\n--------\n")
(stream-for-each pretty-print z)
