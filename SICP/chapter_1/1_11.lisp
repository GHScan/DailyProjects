#lang racket
(require racket/pretty)

(define (f1 n)
  (cond
    ((< n 3) n)
    (else (+ (f1 (- n 1)) (* 2 (f1 (- n 2))) (* 3 (f1 (- n 3)))))
    )
  )

(define (f2 n)
  (define (iter prepre pre cur pos)
    (if (= pos 0)
      cur
      (iter pre cur (+ (* 3 prepre) (* 2 pre) cur) (- pos 1)))
    )

  (
   cond
   ((< n 3) n)
   (else (iter 0 1 2 (- n 2)))
   )
  )

(pretty-print (f1 6))
(pretty-print (f1 7))
(pretty-print (f1 8))

(pretty-print (f2 6))
(pretty-print (f2 7))
(pretty-print (f2 8))
