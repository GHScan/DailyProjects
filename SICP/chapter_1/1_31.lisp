#lang racket
(require racket/pretty)

(define (reduce op map begin end init)
  (cond
    ((< begin end)
     (reduce op map (+ begin 1) end (op init (map begin))))
    (else 
      init)
    )
  )

(define (factorial a b) (reduce * identity a b 1))
(define (mypi n)
 (define (map i) (/ (+ 2 (* 2 (floor (/ i 2)))) (+ 1 (* 2 (ceiling (/ i 2))))))
 (* 4.0 (reduce * map 1 n 1))
 )

(pretty-print (factorial 1 10))
(pretty-print (mypi 1000))
