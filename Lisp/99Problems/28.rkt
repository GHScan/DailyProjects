#lang racket

(define (my-quick-sort l key)
  (if (empty? l)
    empty
    (append (my-quick-sort (filter (lambda (v) (< (key v) (key (car l)))) (cdr l)) key)
            (list (car l)) 
            (my-quick-sort (filter (lambda (v) (<= (key (car l)) (key v))) (cdr l)) key))) 
  )

(define (group l key)
  (let iter ([v-list (list (car l))][l (cdr l)])
    (cond 
      [(empty? l) (list v-list)]
      [(= (key (car v-list)) (key (car l))) (iter (cons (car l) v-list) (cdr l))]
      [else (cons v-list (iter (list (car l)) (cdr l)))]))
  )

(define (lsort l)
  (my-quick-sort l length)
  )

(define (lfsort l)
  (apply append (lsort (group (lsort l) length)))
  )

;------------------------------

(lsort '((a b c) (d e) (f g h) (d e) (i j k l) (m n) (o)))
(lfsort '((a b c) (d e) (f g h) (d e) (i j k l) (m n) (o)))
