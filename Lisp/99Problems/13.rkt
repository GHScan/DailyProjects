#lang racket

(define (encode-direct l)
  (define (pack-unit v count)
    (if (= 1 count) v (list v count))
    )
  (if (empty? l)
    empty
    (let iter ([v (car l)][count 1][l (cdr l)])
      (cond
        [(empty? l) (list (pack-unit v count))]
        [(not (equal? v (car l))) (cons (pack-unit v count) (iter (car l) 1 (cdr l)))]
        [else (iter v (+ count 1) (cdr l))])))
  )
;------------------------------
(encode-direct '())
(encode-direct '(a))
(encode-direct '(a b))
(encode-direct '(a a b))
(encode-direct '(a b b))
(encode-direct '(a a b b))
(encode-direct '(a a a a b c c a a d e e e e))
