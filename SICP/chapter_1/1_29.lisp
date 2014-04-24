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

(define (even n) (= 0 (remainder n 2)))

(define (integral f a b dx)
  (define (map i) (f (+ a (/ dx 2) (* i dx))))
  (* dx (reduce + map 0 (floor (/ (- b a) dx)) 0))
  )

(define (simpson f a b n)
  (define h (/ (- b a) n))
  (define (y i) (f (+ a (* i h))))
  (define (ky i) (* (y i) (if (even i) 4 2)))
  (/ (* h
        (+ (reduce + ky 1 (- n 1) 0.0)
           (y 0)
           (y n))
        )
     3)
  )

(define (cube x) (* x x x))

(pretty-print (integral cube 0 1 0.001))
(pretty-print (simpson cube 0 1 100))
(pretty-print (simpson cube 0 1 1000))
