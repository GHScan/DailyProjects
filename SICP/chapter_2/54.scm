#lang racket

(define (equal? a b)
  (cond
    ((and (number? a) (number? b)) (= a b))
    ((and (boolean? a) (boolean? b)) (eq? a b))
    ((and (symbol? a) (symbol? b)) (eq? a b))
    ((and (string? a) (string? b)) (string=? a b))
    ((and (pair? a) (pair? b)) (and (equal? (car a) (car b)) (equal? (cdr a) (cdr b))))
    ((and (null? a) (null? b)) true)
    (else (error "invalid type!" a b)))
  )

(pretty-print (equal? 1 (- 3 2)))
(pretty-print (equal? 1.0 (- 3.5 2.5)))
(pretty-print (equal? (expt 2 10) (expt 2 10)))
(pretty-print (equal? (expt 2 200) (expt 2 200)))
(pretty-print (equal? "aaa" (make-string 3 #\a)))
(pretty-print (equal? 'a 'a))
(pretty-print (equal? (list 'a 1 2 3 '(1 2)) '(a 1 2 3 (1 2))))
