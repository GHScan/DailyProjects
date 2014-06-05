#lang racket

(require "utils.rkt")

(define (combination l m)
  (if (zero? m)
    (list empty)
    (flatten-map 
      (lambda (v)
        (let ([rest (combination (remove v l) (- m 1))])
          (map (lambda (sub) (cons v sub)) rest)))
      l))
  )

(define (list-subtract l1 l2)
  (cond
    [(empty? l1) empty]
    [(member (car l1) l2) (list-subtract (cdr l1) l2)]
    [else (cons (car l1) (list-subtract (cdr l1) l2))])
  )

(define (group l size-list)
  (if (empty? size-list)
    (list empty)
    (flatten-map
      (lambda (group1)
        (let ([rest (group (list-subtract l group1) (cdr size-list))])
          (map (lambda (group-rest) (cons group1 group-rest)) rest)))
      (combination l (car size-list))))
  )

(define (pandigital? groups)
  (= (list->number (caddr groups)) (* (list->number (car groups)) (list->number (cadr groups))))
  )
;------------------------------
(define groups
  (append 
    (filter pandigital? (group (range 1 10) '(1 4 4)))
    (filter pandigital? (group (range 1 10) '(2 3 4)))))

(apply +
       (uniq-list (map (lambda (group) (list->number (caddr group))) groups)))
