#lang racket

(define (collatz-length n)
  (let iter ([n n][len 1])
    (cond
      [(= 1 n) len]
      [(even? n) (iter (/ n 2) (+ len 1))]
      [else (iter (+ 1 (* n 3)) (+ len 1))]))
  )

(define (solve max-n)
  (let iter ([i 2][longest (list 1 1)])
    (if (> i max-n)
      longest
      (let ([len (collatz-length i)])
        (if (> len (cadr longest))
          (iter (+ i 1) (list i len))
          (iter (+ i 1) longest)))))
  )

(solve 1000000)
