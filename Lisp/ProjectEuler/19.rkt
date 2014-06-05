#lang racket

(require "utils.rkt")

(define (leap-year? year)
  (and (divisible? year 4) (or (not (divisible? year 100)) (divisible? year 400)))
  )

(define leap-months '(1 3 5 7 8 10 12))

(define (month-days year month)
  (cond
    [(member month leap-months) 31]
    [(= 2 month) (if (leap-year? year) 29 28)]
    [else 30])
  )

(define (count-special-days-from-base-date target-year target-month)
  (let iter ([year 1900][month 1][week-days 0][result 0])
    (if (and (= year target-year) (= month target-month))
      result
      (let ([new-year (if (= month 12) (+ year 1) year)]
            [new-month (if (= month 12) 1 (+ month 1))]
            [new-week-days (remainder (+ week-days (month-days year month)) 7)]
            [new-result (if (= 6 week-days) (+ 1 result) result)])
        (iter new-year new-month new-week-days new-result))))
  )

(-
  (count-special-days-from-base-date 2000 12)
  (count-special-days-from-base-date 1901 1))
