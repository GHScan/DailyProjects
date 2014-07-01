(define (add n)
  (if (= n 0) 0 (+ 1 (add (- n 1))))
 )

(add 20 0)
