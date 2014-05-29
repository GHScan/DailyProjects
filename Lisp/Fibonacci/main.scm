#lang racket

(define (vec*vec v1 v2)
  (foldr + 0 (map * v1 v2))
  )
(define (matrix*vec m v)
  (map (curry vec*vec v) m)
  )
(define (matrix-transpose m)
  (apply map list m)
  )
(define (matrix*matrix m1 m2)
  (matrix-transpose (map (curry matrix*vec m1) (matrix-transpose m2)))
  )
(define (matrix-identity n)
  (build-list n (lambda (i) (build-list n (lambda (j) (if (= i j) 1 0)))))
  )
(define (matrix-pow m n)
  (cond 
    ((= 0 n) (matrix-identity (length m)))
    (else 
      (let ((half (matrix-pow m (quotient n 2))))
        (if (even? n)
          (matrix*matrix half half)
          (matrix*matrix m (matrix*matrix half half))))))
  )

(define (fib n)
  (car (matrix*vec (matrix-pow '((0 1)(1 1)) n) '(0 1)))
  )

(pretty-print (build-list 20 fib))
(pretty-print (fib 1000))
