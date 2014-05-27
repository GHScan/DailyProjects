#lang racket

(require "35.rkt")

(define (compile-require exp)
  (let ([cexp (compile (cadr exp))])
    (lambda (env succ fail)
      (cexp 
        env 
        (lambda (b fail2)
          (if b
            (succ empty fail2)
            (fail2)))
        fail)
      ))
  )

(special-form-register 'require compile-require)

(eval G
      '(begin 
         (let ([x (an-integer-between 1 10)])
           (require (= 0 (remainder x 2)))
           x)
         ))
