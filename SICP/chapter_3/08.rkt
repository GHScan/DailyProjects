#lang racket

(define (make-f value)
  (lambda (x) (let ((old value)) (set! value x) old))
  )

(define f (make-f 0))
(let ((x1 (f 0)))
  (let ((x2 (f 1)))
    (+ x1 x2)))

(set! f (make-f 0))
(let ((x1 (f 1)))
  (let ((x2 (f 0)))
    (+ x1 x2)))
