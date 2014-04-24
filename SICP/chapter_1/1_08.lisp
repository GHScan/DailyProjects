#lang racket
(require racket/pretty)

(define (sqrt3 n)
  (define (good_enough guess)
    (< 
      (abs (- 
             (* guess guess guess)
             n)) 
      0.0001)
    )
  (define (next_guess guess)
    (/ 
      (+ 
        (/ n 
           (* guess guess)) 
        (* 2 guess)) 
      3)
    )
  (define (recursive_sqrt3 guess)
    (if (good_enough guess)
      guess
      (recursive_sqrt3 (next_guess guess)))
    )

  (recursive_sqrt3 1.0)
  )

(pretty-print (sqrt3 1))
(pretty-print (sqrt3 2))
(pretty-print (sqrt3 3))
(pretty-print (sqrt3 4))
(pretty-print (sqrt3 5))
(pretty-print (sqrt3 8))
(pretty-print (sqrt3 27))
