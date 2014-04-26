#lang racket
(require racket/pretty)

(define (accumulate f init l)
  (cond 
    ((null? l) init)
    (else (f (car l) (accumulate f init (cdr l)))))
  )

(define (accumlate-n f init lists)
  (cond 
    ((null? (car lists)) (list))
    (else 
      (cons (accumulate f init (map car lists))
            (accumlate-n f init (map cdr lists))))
    )
  )

(define x (list (list 1 2 3) (list 4 5 6) (list 7 8 9)))
(pretty-print (accumlate-n + 0 x))
