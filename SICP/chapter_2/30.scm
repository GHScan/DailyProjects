#lang racket
(require racket/pretty)

(define (square x)
  (* x x)
  )

(define (square-tree tree)
  (cond
    ((null? tree) tree)
    ((not (pair? tree)) (square tree))
    (else (cons (square-tree (car tree)) (square-tree (cdr tree))))
    )
  )

(define (square-tree2 tree)
  (map (lambda (subtree) 
         (cond
           ((not (pair? subtree)) (square subtree))
           (else (square-tree2 subtree)))) 
       tree)
  )

(define x (list 1 (list 2 (list 3 4) 5) (list 6 7)))
(pretty-print (square-tree x))
(pretty-print (square-tree2 x))
