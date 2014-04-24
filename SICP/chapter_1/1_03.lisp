#lang racket
(require racket/pretty)

(define (sum_of_max2 x y z)
  (- (+ x y z)
     (min x (min y z)))
  )

(pretty-print (sum_of_max2 1 2 3))
(pretty-print (sum_of_max2 2 1 3))
(pretty-print (sum_of_max2 3 1 2))
(pretty-print (sum_of_max2 3 2 1))
(pretty-print (sum_of_max2 3 2 2))
(pretty-print (sum_of_max2 3 3 2))
