#lang racket

(require "utils.rkt")

(define nums-3 (stream-map (curry * 3) (in-naturals 1)))
(define nums-5 (stream-map (curry * 5) (in-naturals 1)))
(define nums (stream-merge identity nums-3 nums-5))

;(stream-fold + 0 (stream-take-until nums (lambda (n) (>= n 10))))
(stream-fold + 0 (stream-take-until nums (lambda (n) (>= n 1000))))
