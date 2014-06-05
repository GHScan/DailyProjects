#lang racket

(require "utils.rkt")

(define fibs (stream-cons 1 (stream-cons 1 (stream-map + fibs (stream-rest fibs)))))
(define compare-number (expt 10 999))
(stream-first 
  (stream-filter (lambda (pair) (>= (car pair) compare-number)) 
                 (stream-map cons fibs (in-naturals 1))))
