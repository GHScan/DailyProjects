#lang racket

(require "utils.rkt")

(define (perm l)
  (if (empty? l)
    (list empty)
    (flatten-map 
      (lambda (first)
        (let ([rest (perm (remove first l))])
          (map (lambda (sub) (cons first sub)) rest)))
      l)))

(list-ref (perm (range 10)) 999999)
