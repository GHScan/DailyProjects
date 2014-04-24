#lang racket

(define (sum_of_max2 x y z)
  (cond 
   ((and (<= z x) (<= z y)) (+ x y))
   ((and (<= x y) (<= x z)) (+ y z))
   ((and (<= y x) (<= y z)) (+ x z))
   )
 )

(define (_min x y)
  (if (< x y) x y)
 )
(define (sum_of_max2_v2 x y z)
 (- 
  (+ x y z)
  (_min x (_min y z))
  )
 )

(print (sum_of_max2 1 2 3))
(print (sum_of_max2 2 1 3))
(print (sum_of_max2 3 1 2))
(print (sum_of_max2 3 2 1))
(print (sum_of_max2 3 2 2))
(print (sum_of_max2 3 3 2))
