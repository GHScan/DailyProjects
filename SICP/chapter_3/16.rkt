#lang racket

(define (count-pairs x)
  (if (not (mpair? x))
    0
    (+ 1 
       (count-pairs (mcar x)) 
       (count-pairs (mcdr x))))
  )

(count-pairs (mcons 1 (mcons 2 (mcons 3 empty))))

(let ((a (mcons 1 empty)))
  (define b (mcons 2 a))
  (define c (mcons a b))
  (count-pairs c)
  )

(let ((a (mcons 1 empty)))
  (define b (mcons a a))
  (define c (mcons b b))
  (count-pairs c)
  )

(let ((a (mcons 1 empty)))
  (set-mcdr! a a)
  (count-pairs a)
  )

