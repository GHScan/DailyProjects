#lang racket

(define (sum_of_max2 x y z)
  (cond 
   ((and (<= z x) (<= z y)) (+ x y))
   ((and (<= x y) (<= x z)) (+ y z))
   ((and (<= y x) (<= y z)) (+ x z))
   )
 )

(print (sum_of_max2 1 2 3))
(print (sum_of_max2 2 1 3))
(print (sum_of_max2 3 1 2))
(print (sum_of_max2 3 2 1))
(print (sum_of_max2 3 2 2))
(print (sum_of_max2 3 3 2))
