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

(define (pi k)
  (/ 1 
     (cont-frac (lambda (i) 1.0) (lambda (i) 1.0) k))
  )

(pretty-print (pi 9))
