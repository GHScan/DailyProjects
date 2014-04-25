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

(define (tan-cf x k)
 (define (n i) (if (= i 1) x (- (* x x))))
 (define (d i) (- (* 2 i) 1))
 (cont-frac n d k)
  )

(pretty-print (tan-cf (/ pi 4) 10))
