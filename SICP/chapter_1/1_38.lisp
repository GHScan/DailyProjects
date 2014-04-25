#lang racket
(require racket/pretty)

(define (cont-frac n d k)
  (define (cont-frac-iter n d k nprev)
    (cond
      ((= 0 k)
       (/ (n 1) nprev))
      (else 
        (cont-frac-iter n d (- k 1) (+ (d k) (/ (n (+ k 1)) nprev)))))
    )
  (cont-frac-iter n d (- k 1) (n k))
  )

(define (div a b) 
 (floor (/ a b))
 )

(define (e k)
 (define (n i) 1.0)
 (define (d i) 
  (cond 
   ((= 2 (remainder i 3)) (+ 2 (* 2 (div i 3))))
   (else 1)))
 (+ 2
    (cont-frac n d k))
 )

(pretty-print (e 9))
