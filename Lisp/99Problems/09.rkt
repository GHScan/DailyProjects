#lang racket

(define (pack l)
  (if (empty? l)
    empty
    (let iter ([v-list (list (car l))][l (cdr l)])
      (cond
        [(empty? l) (list v-list)]
        [(not (equal? (car v-list) (car l))) (cons v-list (iter (list (car l)) (cdr l)))]
        [else (iter (cons (car l) v-list) (cdr l))])))
  )
;------------------------------
(pack '())
(pack '(a))
(pack '(a b))
(pack '(a a b))
(pack '(a b b))
(pack '(a a b b))
(pack '(a a a a b c c a a d e e e e))
