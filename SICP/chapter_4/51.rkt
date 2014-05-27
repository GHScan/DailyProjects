#lang racket

(require "35.rkt")

(define (compile-permanent-set! exp)
  (let ([loc (make-variable-location (cadr exp))][cexp (compile (caddr exp))])
    (lambda (env succ fail)
      (cexp 
        env 
        (lambda (value fail2)
          (variable-location-write loc env value)
          (succ empty fail2))
        fail)
      ))
  )

(special-form-register 'permanent-set! compile-permanent-set!)

(eval G
      '(begin
         (define count 0)
         (let ([x (an-integer-between 1 3)][y (an-integer-between 1 3)])
           (permanent-set! count (+ count 1))
           (assert (not (eq? x y)))
           (list x y count))
         ))
