#lang racket
(require racket/pretty)

(define (fast_feb n)
  (define (even n) (= 0 (remainder n 2)))
  (define (div a b) (floor (/ a b)))
  (define (recursive_feb a b p q n)
   (cond 
    ((= n 0) b)
    ((even n)
     (recursive_feb a b 
                    (+ (* p p) (* q q))
                    (+ (* q q) (* 2 p q))
                    (div n 2)))
    (else 
     (recursive_feb 
       (+ (* a (+ p q)) (* b q))
       (+ (* b p) (* a q))
       p q (- n 1)))
    )
    )
  (recursive_feb 1 0 0 1 n)
  )

(pretty-print (fast_feb 2))
(pretty-print (fast_feb 3))
(pretty-print (fast_feb 4))
(pretty-print (fast_feb 5))
(pretty-print (fast_feb 6))
(pretty-print (fast_feb 20))
