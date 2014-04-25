#lang racket
(require racket/pretty)

(define (reduce op map begin end init)
  (cond 
    ((< begin end) (reduce op map (+ begin 1) end (op init (map begin))))
    (else init))
  )

(define (even? n)
  (= 0 (remainder n 2))
  )

(define (compute-pi n)
  (let ((fi (lambda (i) ((if (even? i) - +) (/ 1 (- (* 2.0 i) 1)))))
        )
    (* 4 (reduce + fi 1 n 0)))
  )

(pretty-print (compute-pi 1000))
