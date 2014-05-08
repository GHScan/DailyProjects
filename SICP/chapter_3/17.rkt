#lang racket

(define (set-add set v)
  (cond 
    ((null? set) (list v))
    ((eq? (car set) v) (error "exist!" v))
    (else (cons (car set) (set-add (cdr set) v))))
  )
(define (set-contain? set v)
  (cond
    ((null? set) false)
    ((eq? v (car set)) true)
    (else (set-contain? (cdr set) v)))
  )
(define (count-pairs x)
  (define (iter x set)
    (cond
      ((not (mpair? x)) set)
      ((set-contain? set x) set)
      (else (iter (mcdr x) 
                  (iter (mcar x) 
                        (set-add set x)))))
    )
  (length (iter x empty))
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

