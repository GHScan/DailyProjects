#lang racket

(define (slice l first last)
  (cond
    [(> first 1) (slice (cdr l) (- first 1) (- last 1))]
    [(> last 1) (cons (car l) (slice (cdr l) first (- last 1)))]
    [else (list (car l))])
  )
;------------------------------
(slice '(a b c d) 2 4)
(slice '(a b c d e) 2 4)
(slice '(a b c d e f g h i k) 3 7)
