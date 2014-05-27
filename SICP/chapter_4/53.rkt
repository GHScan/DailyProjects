#lang racket

(require "35.rkt")
(require "51.rkt")
(require "52.rkt")
(require math/number-theory)

(builtin-register 'prime? prime?)

(eval G
      '(begin
         (define (prime-sum-pair l1 l2)
           (let ([n1 (one-of l1)][n2 (one-of l2)])
             (assert (prime? (+ n1 n2)))
             (list n1 n2))
           )
         (let ([pairs empty])
           (if-fail (let ([p (prime-sum-pair '(1 3 5 8) '(20 35 110))])
                      (permanent-set! pairs (cons p pairs))
                      (amb))
                    pairs)))
      )
