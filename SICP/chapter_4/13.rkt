#lang racket

(require "01.rkt")

(define (eval-make-unbound! env exp)
  (env-undefine-variable! env (car exp))
  )
(special-form-add 'make-unbound! eval-make-unbound!)

(run-test)
(eval G
 '(let ((x 1)(y 2))
     (make-unbound! x)
     (+ x y)))
