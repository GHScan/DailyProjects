#lang racket

(require "combinator.rkt")
;------------------------------
; scanner

(define num (c-atom-in (string->list  "0123456789")))
(define alpha (c-atom-in (string->list "_ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz")))
(define alpha-num (c-or num alpha))

(define (token parser)
  (c-select (c-and parser (c-repeat0 (c-atom-in (string->list " \t\n")))) car)
  )

(define NUMBER (token
                 (c-select (c-repeat num) (compose string->number list->string))))
(define ID (token
             (c-select (c-and alpha (c-repeat0 alpha-num)) (lambda (args) (string->symbol (list->string (cons (car args) (cadr args))))))))
(define STRING (token
                 (c-select (c-and (c-atom #\") (c-repeat0 (c-atom-not (c-atom #\"))) (c-atom #\")) (lambda (args) (list->string (cadr args))))))
(define (RESV a) 
  (token 
    (c-select (c-atom-list (string->list (symbol->string a))) (compose string->symbol list->string)))
  )
;------------------------------
; parser

(c-lazy Parenthesis-Exp 
        (c-select (c-and (RESV '\() Exp (RESV '\))) cadr))
(c-lazy Factor (c-or 
                 NUMBER
                 ID
                 STRING
                 Parenthesis-Exp))
(c-lazy MulDiv (c-select
                 (c-and
                   Factor
                   (c-repeat0 (c-and (c-or (RESV '*) (RESV '/)) Factor)))
                 (lambda (args) 
                   (foldl (lambda (op-right left) (list (car op-right) left (cadr op-right))) (car args) (cadr args)))))
(c-lazy Exp (c-select
              (c-and
                MulDiv
                (c-repeat0 (c-and (c-or (RESV '+) (RESV '-)) MulDiv)))
              (lambda (args) 
                (foldl (lambda (op-right left) (list (car op-right) left (cadr op-right))) (car args) (cadr args)))))
(c-lazy Program (c-and Exp c-eof))
;------------------------------
(define program "1 + 2 * (3 - 4) / 5 - 2")
(Program (string->list program) 
         (lambda (ast rest) (printf "Parse success: ~a\n" ast)) 
         (lambda (rest) (printf "Parser faild: ~a\n" rest)))
