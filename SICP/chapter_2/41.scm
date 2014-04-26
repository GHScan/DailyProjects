#lang racket
(require racket/pretty)

(define (range first last)
  (cond 
    ((> first last) (list))
    (else (cons first (range (+ first 1) last))))
  )

(define (flatmap f l)
  (foldr append (list) (map f l))
  )

(define (unique-triples n)
  (flatmap
    (lambda (i) (flatmap 
                  (lambda (j) (map 
                                (lambda (k) (list i j k))
                                (range 1 (- j 1))))
                  (range 1 (- i 1))))
    (range 1 n))
  )

(define (sum-triples n s)
 (filter (lambda (l) (= s (+ (car l) (cadr l) (caddr l)))) (unique-triples n))
 )

(pretty-print (sum-triples 8 12))
