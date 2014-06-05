#lang racket

(require "utils.rkt")

(define (loop-numbers n)
  (let ([n-list (number->list n)])
    (uniq-list (map (lambda (i) (list->number (rotate-left n-list i))) (range (length n-list)))))
  )

;------------------------------
(define N 1000000)
(define table (make-prime-table N))

(define loop-primes
  (let iter ([i 2][result empty])
    (cond
      [(>= i N) (apply append result)]
      [(not (vector-ref table i)) (iter (+ i 1) result)]
      [else 
        (let ([nums (loop-numbers i)])
          (if (andmap (lambda (n) (vector-ref table n)) nums)
            (set! result (cons nums result))
            'ok)
          (for-each (lambda (n) (vector-set! table n false)) nums)
          (iter (+ i 1) result))])
    ))

(length loop-primes)
