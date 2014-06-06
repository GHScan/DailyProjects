#lang racket

(require "utils.rkt")

(define N 1000000)

(define table (make-prime-table N))

(define the-primes (filter (lambda (i) (vector-ref table i)) (range 2 N)))
(define the-prime-sum-table
  (list->vector
    (let iter ([l the-primes][result (list 0)])
      (if (empty? l) 
        (reverse result)
        (iter (cdr l) (cons (+ (car l) (car result)) result))))))

(define (prime-sum start end)
  (- (vector-ref the-prime-sum-table end) (vector-ref the-prime-sum-table start))
  )

(define max-length 1)
(define max-sum 0)
(do ([start 0 (+ 1 start)])
  ((>= start (vector-length the-prime-sum-table)) )
  (do ([end (+ start max-length) (+ 1 end)])
    ((or (>= end (vector-length the-prime-sum-table)) (>= (prime-sum start end) N)) )
    (let ([sum (prime-sum start end)])
      (if (vector-ref table sum)
        (begin 
          (set! max-length (- end start))
          (set! max-sum sum))
        'ok))))

max-length
max-sum
