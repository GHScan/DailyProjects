#lang racket

(require "01.rkt")

(eval G
      '(let ((a 1))
         (define (f x)
           (define b (+ a x))
           (define a 5)
           (+ a b))
         (f 10)))
