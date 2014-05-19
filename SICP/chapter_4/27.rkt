#lang racket

(require "25.rkt")

(eval G
 '(begin
     (define count 0)
     (define (id x)
      (set! count (+ count 1))
      x)
     (define w (id (id 10)))
     (pretty-print count)
     (pretty-print w)
     (pretty-print count)
     ))
