#lang racket
(require racket/pretty)

(define (square x)
  (* x x)
  )

(define (tree-map f tree)
  (cond
    ((null? tree) tree)
    ((not (pair? tree)) (f tree))
    (else (cons (tree-map f (car tree)) (tree-map f (cdr tree))))
    )
  )
(define (square-tree tree) (tree-map square tree))

(define x (list 1 (list 2 (list 3 4) 5) (list 6 7)))
(pretty-print (square-tree x))
