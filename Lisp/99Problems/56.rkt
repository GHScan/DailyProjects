#lang racket

(define (mirror a b)
  (cond
    [(empty? a) (empty? b)]
    [(empty? b) (empty? a)]
    [else (and (mirror (cadr a) (caddr b)) (mirror (caddr a) (cadr b)))])
  )

(define (symmetirc a)
  (cond
    [(empty? a) false]
    [else (mirror (cadr a) (caddr a))])
  )
;------------------------------
(symmetirc '(x (x () ()) ()))
(symmetirc '(x (x () ()) (x () ())))
