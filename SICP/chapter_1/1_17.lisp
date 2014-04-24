#lang racket
(require racket/pretty)

(define (fast_expt a b)

  (define (even? n) (= (remainder n 2) 0))
  (define (fast_expt_iter _b order result)
    (if (= _b 0)
      result
      (fast_expt_iter
        (floor (/ _b 2))
        (+ order order)
        (+ result 
           (if (even? _b) 0 order))
        )
      )
    )

  (fast_expt_iter b a 0)
  )

(pretty-print (fast_expt 2 5))
(pretty-print (fast_expt 2 32))
(pretty-print (fast_expt 2 33))
(pretty-print (fast_expt 10 7))
(pretty-print (fast_expt 10 8))
