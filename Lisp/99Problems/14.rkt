#lang racket

(define (dupli l)
  (if (empty? l)
    empty
    (cons (car l) (cons (car l) (dupli (cdr l)))))
  )
;------------------------------
(dupli '())
(dupli '(a))
(dupli '(a b))
(dupli '(a b c))
(dupli '(a b c c d))
