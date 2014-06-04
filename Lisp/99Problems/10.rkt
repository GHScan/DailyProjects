#lang racket

(define (encode l)
  (if (empty? l)
    empty
    (let iter ([v (car l)][count 1][l (cdr l)])
      (cond
        [(empty? l) (list (list v count))]
        [(not (equal? v (car l))) (cons (list v count) (iter (car l) 1 (cdr l)))]
        [else (iter v (+ count 1) (cdr l))])))
  )
;------------------------------
(encode '())
(encode '(a))
(encode '(a b))
(encode '(a a b))
(encode '(a b b))
(encode '(a a b b))
(encode '(a a a a b c c a a d e e e e))
