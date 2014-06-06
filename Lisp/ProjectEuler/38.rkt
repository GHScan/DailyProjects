#lang racket

(require "utils.rkt")

(define the-1-9 (range 1 10))
(define (pandigital? num-list)
  (equal? the-1-9 (sort num-list <))
  )

(define (find-pandigital num)
  (let iter ([i 1][result empty])
    (if (>= (length result) 9)
      (if (pandigital? result) result empty)
      (iter (+ i 1) (append result (number->list (* num i))))))
  )

(define (search init-num-list)
  (filter (lambda (l) (not (empty? l))) (map find-pandigital (range (list->number init-num-list) (expt 10 (length init-num-list)))))
  )
;------------------------------
(define max-number-list '(9 1 8 2 7 3 6 4 5))

(do ([w 2 (+ w 1)])
  ((> w 4))
  (let ([result (search (take max-number-list w))])
    (set! max-number-list 
      (number->list (apply max (map list->number (cons max-number-list result)))))))

max-number-list
