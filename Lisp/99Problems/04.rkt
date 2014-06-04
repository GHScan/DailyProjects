#lang racket

(define (my-length l)
  (let iter ([l l][len 0])
    (if (empty? l)
      len
      (iter (cdr l) (+ len 1))))
  )
;------------------------------
(my-length '())
(my-length '(a))
(my-length '(a b))
(my-length '(a b c))
(my-length '(a b c d))
