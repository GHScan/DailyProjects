#lang racket

(require "utils.rkt")

(define (amicable-numbers max-n)
  (filter 
    (lambda (i) (< i max-n))
    (let iter ([i 2][checked (list->set empty)][result empty])
      (cond
        [(>= i max-n) result]
        [(set-member? checked i) (iter (+ i 1) checked result)]
        [else 
          (let ([j (apply + (proper-divisors i))])
            (if (and (not (= i j)) (= i (apply + (proper-divisors j)))) 
              (iter (+ i 1) (set-add (set-add checked i) j) (cons i (cons j result)))
              (iter (+ i 1) (set-add (set-add checked i) j) result)))])))
  )

(apply + (amicable-numbers 10000))
