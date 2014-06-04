#lang racket

(define (encode-modified l)
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
(encode-modified '())
(encode-modified '(a))
(encode-modified '(a b))
(encode-modified '(a a b))
(encode-modified '(a b b))
(encode-modified '(a a b b))
(encode-modified '(a a a a b c c a a d e e e e))
