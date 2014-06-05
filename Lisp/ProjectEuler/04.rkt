#lang racket

(require "utils.rkt")

(define (palindrome-number? n)
  (let ([s (string->list (number->string n))])
    (equal? s (reverse s)))
  )

;(palindrome-number? 9008)
(apply 
  max
  (list-comprehension
    (* x y)
    (x <- (range 100 1000)) 
    (y <- (range x 1000))
    (palindrome-number? (* x y))))
