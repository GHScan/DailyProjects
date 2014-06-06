#lang racket

(require "utils.rkt")

(define num-factors
  (stream-map (lambda (n) (list n (length (find-prime-factors-pair n)))) (in-naturals 1)))
(define num-group-by-factors 
  (stream-group-by cadr num-factors))
(define continue-group
  (stream-filter (lambda (group) (= (cadr (car group)) (length group))) num-group-by-factors))

(stream->list (stream-take (stream-filter (lambda (group) (= 4 (length group))) continue-group) 1))
