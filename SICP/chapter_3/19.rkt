#lang racket
(require scheme/mpair)

(define (make-cycle x)
  (set-mcdr! (last-pair x) x)
  x)
(define (last-pair x)
  (if (null? (mcdr x))
    x
    (last-pair (mcdr x)))
  )

(define (loop? x)
  (define (exist x v)
    (cond
      ((null? x) false)
      ((eq? x v) true)
      (else (exist (mcdr x) v)))
    )
  (exist (mcdr x) x)
  )

(loop? (mlist 1))
(loop? (mlist 1 2))
(loop? (mlist 1 2 3))
(loop? (make-cycle (mlist 1)))
(loop? (make-cycle (mlist 1 2)))
(loop? (make-cycle (mlist 1 2 3)))
