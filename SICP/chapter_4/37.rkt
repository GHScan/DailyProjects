#lang racket

(require "35.rkt")

(builtin-register 'sqrt sqrt)
(builtin-register 'integer? integer?)
(builtin-register 'floor floor)

(eval G 
      '(begin
         (define (an-pythagorean-triple-between low high)
           (let ([k (an-integer-between low high)])
             (let ([j (an-integer-between low (- k 1))])
               (let ([inexact-i (sqrt (- (* k k) (* j j)))])
                 (assert (integer? inexact-i))
                 (list (floor inexact-i) j k))))
           )
         (an-pythagorean-triple-between 1 100)
         ))
