#lang racket

(define (prime-factors n)
  (let iter ([i 2][n n])
    (cond
      [(= n 1) empty]
      [(> (* i i) n) (list (list n 1))]
      [(= 0 (remainder n i)) 
       (do ([n n (quotient n i)][count 0 (+ count 1)])
         ((not (= 0 (remainder n i))) (cons (list i count) (iter (+ i 1) n))))]
      [else (iter (+ i 1) n)]))
  )
;------------------------------
(map prime-factors (range 1 20))
(prime-factors 315)
