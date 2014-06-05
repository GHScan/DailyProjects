#lang racket

(define (mirror? a b)
  (cond
    [(empty? a) (empty? b)]
    [(empty? b) (empty? a)]
    [else (and (mirror? (cadr a) (caddr b)) (mirror? (caddr a) (cadr b)))])
  )

(define (symmetirc? a)
  (cond
    [(empty? a) false]
    [else (mirror? (cadr a) (caddr a))])
  )

(define (insert v node)
  (cond
    [(empty? node) (list v empty empty)]
    [(= v (car node)) node]
    [(< v (car node)) (list (car node) (insert v (cadr node)) (caddr node))]
    [else (list (car node) (cadr node) (insert v (caddr node)))]
    )
  )

(define (construct l)
  (foldl insert empty l)
  )
;------------------------------
(symmetirc? (construct '(5 3 18 1 4 12 21)))
(symmetirc? (construct '(3 2 5 7 1)))
