#lang racket

(provide totient-phi-2)
;------------------------------
(define (prime-factors n)
  (let iter ([i 2][n n])
    (cond
      [(= n 1) empty]
      [(> (* i i) n) (list (list n 1))]
      [(= 0 (remainder n i)) 
       (do ([n n (quotient n i)][count 0 (+ count 1)])
         ((not (= 0 (remainder n i))) (cons (list i count) (iter (+ i 1) n))))]
      [else (iter (+ i 1) n)]))
  )

(define (totient-phi-2 n)
  (foldl (lambda (pair init) 
           (let ([p (car pair)][m (cadr pair)])
             (+ init (* (- p 1) (expt p (- m 1)))))) 
         0 
         (prime-factors n))
  )
;------------------------------
(map (lambda (i) (list i (totient-phi-2 i))) (range 2 32))
