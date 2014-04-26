#lang racket

(define (gcd x y)
 (cond 
  ((= 0 y) x)
  (else (gcd y (remainder x y))))
 )

(define (rat-create x y)
  (cond
    ((< y 0) (rat-create (- x) (- y)))
    (else
      (let ((g (gcd x y)))
        (cons (/ x g) (/ y g)))
      ))
  )

(define (rat-numer x)
  (car x)
  )

(define (rat-denom x) 
  (cdr x)
  )

(define (rat-print x)
  (newline)
  (display (rat-numer x))
  (cond 
    ((not (= 1 (rat-denom x)))
     (display "/")
     (display (rat-denom x)))
    )
  )

(define (rat-add x y)
  (rat-create (+ (* (rat-numer x) (rat-denom y)) (* (rat-denom x) (rat-numer y))) 
              (* (rat-denom x) (rat-denom y)))
  )

(define (rat-sub x y)
  (rat-create (- (* (rat-numer x) (rat-denom y)) (* (rat-denom x) (rat-numer y))) 
              (* (rat-denom x) (rat-denom y)))
  )

(define (rat-mul x y)
  (rat-create (* (rat-numer x) (rat-numer y))
              (* (rat-denom x) (rat-denom y)))
  )

(define (rat-div x y)
  (rat-create (* (rat-numer x) (rat-denom y))
              (* (rat-denom x) (rat-numer y)))
  )

(define one-half (rat-create 1 2))
(define one-third (rat-create 1 3))
(define one (rat-create 1 1))
(rat-print one-half)
(rat-print one-third)
(rat-print (rat-mul one-third one-third))
(rat-print (rat-add one-third one-third))
(rat-print (rat-div one-third one-third))
(rat-print (rat-sub one-third one))
