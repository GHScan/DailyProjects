#lang racket

(require "01.rkt")

(define (eval-named-let env exp)
  (let ((name (car exp))
        (arg-names (map car (cadr exp)))
        (arg-values (map cadr (cadr exp)))
        (body (cddr exp)))
    (eval-procedure (eval-lambda 
                      env
                      (list empty
                            (cons 'define (cons (cons name arg-names) body))
                            (cons name arg-values))) empty))
  )
(define (eval-new-let env exp)
  (if (symbol? (car exp))
    (eval-named-let env exp)
    (eval-let env exp))
  )
(special-form-add 'let eval-new-let)

(run-test)
(eval G
      '(begin
         (define (fib n)
           (let fib-iter ((a 0)(b 1)(count n))
             (if (= count 0)
               a
               (fib-iter b (+ a b) (- count 1))))
           )
         (build-list 10 fib)))
