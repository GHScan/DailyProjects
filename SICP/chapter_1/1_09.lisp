#lang racket
(require racket/pretty)

(define (tri_point row col)
  (cond 
    ((or (= 1 col) (= row col)) 
     1)
    (else 
      (+ (tri_point (- row 1) (- col 1)) (tri_point (- row 1) col)))
    )
  )

(pretty-print (tri_point 1 1))
(pretty-print (tri_point 5 1))
(pretty-print (tri_point 5 2))
(pretty-print (tri_point 5 3))
(pretty-print (tri_point 5 4))
(pretty-print (tri_point 5 5))
