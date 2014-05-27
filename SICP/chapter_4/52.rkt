#lang racket

(require "35.rkt")

(define (compile-if-fail exp)
  (let ([succ-cexp (compile (cadr exp))][fail-cexp (compile (caddr exp))])
    (lambda (env succ fail)
      (succ-cexp 
        env
        (lambda (v fail2)
          (succ v fail))
        (lambda ()
          (fail-cexp env succ fail)))))
  )

(special-form-register 'if-fail compile-if-fail)
(builtin-register 'even? even?)


(eval G
      '(begin
         (pretty-print 
           (if-fail (let ([x (one-of '(1 3 5))])
                      (assert (even? x))
                      x)
                    'all-odd))
         (pretty-print 
           (if-fail (let ([x (one-of '(1 3 5 8))])
                      (assert (even? x))
                      x)
                    'all-odd))
         ))
