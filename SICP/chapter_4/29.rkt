#lang racket

(require "25.rkt")

(eval G
 '(begin
     (define (square x) (* x x))
     (define count 0)
     (define (id x) 
      (set! count (+ count 1))
      x)
     (pretty-print (square (id 10)))
     (pretty-print count)
     ))
