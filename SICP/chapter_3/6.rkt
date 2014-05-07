#lang racket

(define (rand-update value)
  (remainder (+ (* value 1103515245) 12345) 4294967296)
  )

(define rand
  (let ((value 0))
    (lambda args
      (cond 
        ((eq? (car args) 'generate) (set! value (rand-update value)) (quotient value 65536))
        ((eq? (car args) 'reset) (set! value (cadr args)) (rand 'generate)))
      ))
  )

(rand 'reset 1234)
(build-list 10 (lambda (x) (rand 'generate)))
(rand 'reset 1234)
(build-list 10 (lambda (x) (rand 'generate)))
