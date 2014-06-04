#lang racket

(define (drop l n)
  (let iter ([l l][i 1])
    (cond
      [(empty? l) empty]
      [(= i n) (iter (cdr l) 1)]
      [else (cons (car l) (iter (cdr l) (+ i 1)))])
    )
  )
;------------------------------
(drop '() 3)
(drop '(a) 3)
(drop '(a b) 3)
(drop '(a b c) 3)
(drop '(a b c d e f g h i k) 3)
