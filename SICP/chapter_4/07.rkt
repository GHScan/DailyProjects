#lang racket

(require "01.rkt")

(define (eval-let* env exp)
  (define (let*->nested-lets name-var-list body)
    (if (null? (cdr name-var-list))
      (cons 'let (cons (list (car name-var-list)) body))
      (list 'let (list (car name-var-list)) (let*->nested-lets (cdr name-var-list) body)))
    )
  (eval env (let*->nested-lets (car exp) (cdr exp)))
  )

(special-form-add 'let* eval-let*) 

(run-test)
(eval G
      '(let* ((x 3)
              (y (+ x 2))
              (z (+ x y 5)))
         (* x z)))
