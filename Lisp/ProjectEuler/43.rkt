#lang racket

(require "utils.rkt")

(define (var-permutation l divisors)
  (let iter ([l l][result empty][divisors divisors])
    (cond
      [(empty? l) (list (reverse result))]
      [else 
        (flatten-map 
          (lambda (v)
            (let* ([new-result (cons v result)][should-check (>= (length new-result) 3)])
              (if (or (not should-check) (divisible? (list->number (reverse (take new-result 3))) (car divisors)))
                (iter (remove v l) new-result (if should-check (cdr divisors) divisors))
                empty)))
          l)]))
  )

(apply + (map list->number (var-permutation (range 10) '(1 2 3 5 7 11 13 17))))
