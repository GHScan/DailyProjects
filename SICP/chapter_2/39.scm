#lang racket
(require racket/pretty)

(define (fold-right f init l)
  (cond
    ((null? l) init)
    (else (f (car l) (fold-right f init (cdr l)))))
  )

(define (fold-left f init l)
  (cond
    ((null? l) init)
    (else (fold-left f (f init (car l)) (cdr l))))
  )

(define (reverse l)
 (fold-left (lambda (init i) (cons i init)) (list) l)
 )
;(define (reverse l)
; (fold-right (lambda (i init) (append init (list i))) (list) l)
; )

(pretty-print (reverse (list)))
(pretty-print (reverse (list 1)))
(pretty-print (reverse (list 1 2)))
(pretty-print (reverse (list 1 2 3 4 5)))
