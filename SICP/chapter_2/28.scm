#lang racket
(require racket/pretty)

(define (fringe this) 
 (cond 
  ((null? this) this)
  ((not (pair? this)) (list this))
  (else (append (fringe (car this)) (fringe (cdr this))))
  )
 )

(pretty-print (fringe (list)))
(pretty-print (fringe (list 1)))
(pretty-print (fringe (list 1 (list 2 3) (list 4) 5)))

(define x (list (list 1 2) (list 3 4)))
(pretty-print (fringe x))
(pretty-print (fringe (list x x)))
