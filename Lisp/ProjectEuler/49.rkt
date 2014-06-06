#lang racket

(require "utils.rkt")

(define (check-group l)
  (if (< (length l) 3)
    empty
    (filter 
      (lambda (l) (= (- (cadr l) (car l)) (- (caddr l) (cadr l))))
      (combination l 3)))
  )

(define table (make-prime-table 10000))

(define w4-primes (filter (lambda (n) (vector-ref table n)) (range 1000 10000)))
(define prime-groups
  (filter
    (compose not empty?)
    (map 
      check-group
      (map 
        (lambda (pair) (sort (cdr pair) <))
        (hash->list 
          (foldl (lambda (n h) 
                   (let ([key (sort (number->list n) <)])
                     (hash-set! h key (cons n (hash-ref h key empty)))
                     h)) 
                 (make-hash) w4-primes))))))
prime-groups
