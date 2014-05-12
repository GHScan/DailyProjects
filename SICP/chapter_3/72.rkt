#lang racket

(require "70.rkt")
(require "71.rkt")

(define (sqr-sum-of-pair pair)
  (+ (sqr (car pair)) (sqr (cadr pair)))
  )

(define sqr-sum-stream 
  (stream-filter (lambda (x) (= 3 (length x)))
                 (stream-groupby sqr-sum-of-pair
                                 (weighted-pairs sqr-sum-of-pair (in-naturals 1) (in-naturals 1)))))

(build-list 10 (lambda (i) (let ((item (stream-ref sqr-sum-stream i))) (list (sqr-sum-of-pair (car item)) item))))
