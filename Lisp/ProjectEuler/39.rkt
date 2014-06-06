#lang racket

(require "utils.rkt")

(define p-count-list
  (map 
    (lambda (l) (cons (apply + (car l)) (length l)))
    (group-by
      (list-comprehension
        (list a b c)
        (a <- (range 1 1000))
        (b <- (range a 1000))
        (c <- (range b (- 1000 a b)))
        (= (sqr c) (+ (sqr a) (sqr b)))
        )
      (lambda (l) (apply + l)))))

(let ([max-count (apply max (map cdr p-count-list))])
  (car (memf (lambda (pc) (= (cdr pc) max-count)) p-count-list)))
