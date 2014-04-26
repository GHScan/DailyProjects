#lang racket
(require racket/pretty)

(define (accumulate f init l)
  (cond 
    ((null? l) init)
    (else (f (car l) (accumulate f init (cdr l)))))
  )

(define (accumulate-n f init lists)
  (cond
    ((null? (car lists)) (list))
    (else
      (cons (accumulate f init (map car lists))
            (accumulate-n f init (map cdr lists)))))
  )

(define (dot-product v w)
  (accumulate + 0 (accumulate-n * 1 (list v w)))
  )

(define (matrix-*-vector m v)
  (map (lambda (l) (dot-product l v)) m)
  )

(define (transpose m)
  (accumulate-n cons (list) m)
  )

(define (matrix-*-matrix m n)
  (let ((trans-n (transpose n)))
    (map (lambda (r) (matrix-*-vector trans-n r)) m)
    )
  )

(define m1 (list (list 1 2) (list 3 4)))
(define m2 (list (list 2 3) (list 3 5)))
(pretty-print (matrix-*-matrix m1 m2))
