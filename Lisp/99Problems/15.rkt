#lang racket

(define (repli l n)
  (let iter ([l l][i 0])
    (cond
      [(empty? l) empty]
      [(= i n) (iter (cdr l) 0)]
      [else (cons (car l) (iter l (+ 1 i)))]))
  )
;------------------------------
(repli '() 3)
(repli '(a) 3)
(repli '(a b) 3)
(repli '(a b c) 3)
