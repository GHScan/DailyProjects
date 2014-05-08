#lang racket

(define (random-in-range low high)
  (+ low (* (random) (- high low)))
  )

(define (monte-carlo trials experiment)
  (define (iter remain passed)
    (if (zero? remain)
      (/ passed trials)
      (iter (- remain 1) (if (experiment) (+ passed 1) passed)))
    )
  (iter trials 0.0)
  )

(define (estimate-integral trials P x1 x2 y1 y2)
  (*  
    (* (- x2 x1) (- y2 y1))
    (monte-carlo trials 
                 (lambda () (P (random-in-range x1 x2) (random-in-range y1 y2)))))
  )

(define (estimate-pi trials)
  (estimate-integral trials (lambda (x y) (<= (+ (sqr x) (sqr y)) 1)) -1 1 -1 1)
 )

(estimate-pi 200000)
