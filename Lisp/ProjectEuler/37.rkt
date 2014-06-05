#lang racket

(require "utils.rkt")

(define table (make-prime-table 10000000))

(define (expand-primes primes expander digits)
  (flatten-map
    (lambda (digit)
      (filter (lambda (prime) (vector-ref table (list->number prime))) 
              (map (lambda (prime) (expander prime digit)) primes)))
    digits)
  )

;------------------------------
(define left-expander (lambda (prime digit) (cons digit prime)))
(define right-expander (lambda (prime digit) (append prime (list digit))))
(define left-digits (range 1 10))
(define right-digits '(1 3 5 7 9))
(define init-primes '((2)(3)(5)(7)))
(define truncatable-primes
  (let iter ([left-primes init-primes][right-primes init-primes]
             [result empty])
    (if (>= (length result) 15)
      result
      (iter 
        (expand-primes left-primes left-expander left-digits) 
        (expand-primes right-primes right-expander right-digits) 
        (append (list-intersect (map list->number left-primes)
                                (map list->number right-primes))
                result)))
    ))

(apply + (filter (curry < 10) truncatable-primes))
