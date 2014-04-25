#lang racket

(define (float-equal? a b) (< (abs (- a b)) 0.001))

(define (even? n)
  (= 0 (remainder n 2))
  )

(define (div a b) (floor (/ a b)))

(define (square n) (* n n))

(define (pow a b)
  (cond 
    ((= b 0) 1)
    ((even? b) (square (pow a (div b 2))))
    (else (* a (pow a (- b 1))))
    )
  )

(define (search-root f a b)
  (let ((mid (/ (+ a b) 2.0))
        (fmid (f (/ (+ a b) 2)))
        )
    (cond 
      ((float-equal? a b) mid)
      ((< fmid 0) (search-root f mid b))
      ((> fmid 0) (search-root f a mid))
      (else mid)))
  )

(define (sqrt-n n x)
  (let ((f (lambda (i) (- x (pow i n))))
        )
    (search-root f x 0))
  )

(pretty-print (sqrt-n 2 2))
(pretty-print (sqrt-n 2 3))
(pretty-print (sqrt-n 3 8))
(pretty-print (sqrt-n 3 27))
(pretty-print (sqrt-n 3 100))
