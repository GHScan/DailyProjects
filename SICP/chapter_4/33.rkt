#lang racket

(require "25.rkt")
(require "32.rkt")

(define (new-compile-quote exp)
  (define (list->cons l)
    (if (null? l)
      'empty
      (list 'cons (car l) (list->cons (cdr l))))
    )
  (let ((e (compile (list->cons (car exp)))))
    (lambda (env) (e env)))
  )
(register-special-form-compiler 'quote new-compile-quote)

(eval G
 '(begin
     (pretty-print (map (curry 2 * 2) '(1 2 3 4)))
     ))
