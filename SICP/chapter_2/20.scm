#lang racket

(define (same-parity . l)
  (define (same-parity-iter l mod2)
    (cond 
      ((null? l) l)
      ((= mod2 (remainder (car l) 2)) (cons (car l) (same-parity-iter (cdr l) mod2)))
      (else (same-parity-iter (cdr l) mod2)))
    )
  (cons (car l)
        (same-parity-iter (cdr l) (remainder (car l) 2)))
  )

(pretty-print (same-parity 1 2 3 4 5 6 7))
(pretty-print (same-parity 2 3 4 5 6 7))
