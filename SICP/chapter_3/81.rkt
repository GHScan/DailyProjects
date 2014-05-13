#lang racket

(provide (all-defined-out))

(define (random-update n)
  (remainder (+ (* n 214013) 2531011) 4294967296)
  )

(define (random-get n)
  (quotient n 65536)
  )

(define (make-random-stream op-stream init-number)
  (if (stream-empty? op-stream)
    empty-stream
    (stream-cons (let ((op (stream-first op-stream)))
                   (cond 
                     ((eq? (car op) 'generate) (random-get init-number)) 
                     ((eq? (car op) 'reset) (set! init-number (random-update (cadr op))) (random-get init-number)) 
                     ))
                 (make-random-stream (stream-rest op-stream) (random-update init-number))))
  )

(define (make-cycle-stream s)
  (define cycle (stream-append s (stream-cons (stream-first s) (stream-rest cycle))))
  cycle)

(define randoms-1 (make-random-stream (make-cycle-stream (stream (list 'generate))) 1))
(build-list 20 (curry stream-ref randoms-1))


(define randoms-1-repeat-5 
  (make-random-stream 
    (stream-map 
      (lambda (i) (if (= 0 (remainder i 5)) (list 'reset 1) (list 'generate))) 
      (in-naturals)) 1))
(build-list 20 (curry stream-ref randoms-1-repeat-5))
