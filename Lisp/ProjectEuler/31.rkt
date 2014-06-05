#lang racket

(define (solve l value)
  (cond
    [(zero? value) 1]
    [(empty? l) 0]
    [else (apply +
                 (map 
                   (lambda (count)
                     (solve (cdr l) (- value (* count (car l)))))
                   (range (+ 1 (quotient value (car l))))))])
  )

(solve '(1 2 5 10 20 50 100 200) 200)
