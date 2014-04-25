#lang racket
(require racket/pretty)

(define (float-equal? a b)
  (< (abs (- a b)) 0.001)
  )

(define (reduce op map begin end init)
  (cond
    ((< begin end) (reduce op map (+ begin 1) end (op init (map begin))))
    (else init))
  )

(define (integral f a b dx)
  (let ((n (floor (/ (- b a) dx)))
        (fi (lambda (i) (f (+ a (* i dx) (/ dx 2)))))
        )
    (* dx
       (reduce + fi 0 n 0)))
  )

(define (sqrt x)
  (define (next-guess guess)
   (/ (+ guess (/ x guess)) 2)
    )
  (define (sqrt-guess guess)
    (cond 
      ((float-equal? (* guess guess) x) 
       guess)
      (else
        (sqrt-guess (next-guess guess))))
    )
  (sqrt-guess 1)
  )

(define (circle x)
  (sqrt (- 1 (* x x)))
  )

(define mypi (* 2 (integral circle -1 1 0.01)))

(pretty-print mypi)
