#lang racket

(define (lotto-select M N)
  (if (= 0 M)
    empty
    (let ([rest (lotto-select (- M 1) (- N 1))]
          [choose (+ 1 (random N))])
      (if (member choose rest)
        (cons N rest)
        (cons choose rest))))
  )
;------------------------------
(lotto-select 1 3)
(lotto-select 2 3)
(lotto-select 3 5)
(lotto-select 4 5)
(lotto-select 6 49)
