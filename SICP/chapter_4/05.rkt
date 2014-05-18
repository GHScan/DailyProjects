#lang racket

(require "01.rkt")

(define (eval-new-cond env exp)
  (let ((case-pred (caar exp))(case-body (cdar exp))(rest (cdr exp)))
    (let ((case-pred-value (eval env case-pred)))
      (if (or (null? rest) case-pred-value)
        (if (eq? '=> (car case-body))
          (eval-procedure (eval-begin env (cdr case-body)) (list case-pred-value)) 
          (eval-begin env case-body))
        (eval-new-cond env rest))))
  )

(special-form-add 'cond eval-new-cond)

(run-test)

(eval G
      '(begin 
         (define (f n)
           (cond
             ((<= n 0) 0)
             ((and (= 0 (remainder n 2)) n) => (curry 2 * 2))
             (else (f (- n 1))))
           )
         (build-list 10 f))
      )
