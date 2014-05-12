#lang racket

(require "70.rkt")
(provide (all-defined-out))

(define (stream-groupby key s)
  (define (iter s repeats)
    (if (= (key (stream-first s)) (key (car repeats)))
      (iter (stream-rest s) (cons (stream-first s) repeats))
      (stream-cons repeats (iter (stream-rest s) (list (stream-first s)))))
    )
  (iter (stream-rest s) (list (stream-first s)))
  )

(define (cube x) (* x x x))
(define (cube-sum-of-pair pair)
  (+ (cube (car pair)) (cube (cadr pair)))
  )

(define ramanujan-stream 
  (stream-filter (lambda (x) (= 2 (length x)))
                 (stream-groupby cube-sum-of-pair
                                 (weighted-pairs cube-sum-of-pair (in-naturals 1) (in-naturals 1)))))

(build-list 10 (lambda (i) (cube-sum-of-pair (car (stream-ref ramanujan-stream i)))))
