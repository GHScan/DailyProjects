#lang racket

(provide (all-defined-out))

(define (merge-weighted weight s0 s1)
  (cond 
    ((stream-empty? s0) s1)
    ((stream-empty? s1) s0)
    (else 
      (let ((s0-car (stream-first s0))(s0-cdr (stream-rest s0))
            (s1-car (stream-first s1))(s1-cdr (stream-rest s1)))
        (cond
          ((< (weight s0-car) (weight s1-car)) (stream-cons s0-car (merge-weighted weight s0-cdr s1)))
          ((< (weight s1-car) (weight s0-car)) (stream-cons s1-car (merge-weighted weight s0 s1-cdr)))
          (else (stream-cons s0-car (stream-cons s1-car (merge-weighted weight s0-cdr s1-cdr))))
          ))))
  )

(define (weighted-pairs weight s0 s1)
  (if (or (stream-empty? s0) (stream-empty? s1))
    empty-stream
    (stream-cons 
      (list (stream-first s0) (stream-first s1))
      (merge-weighted  weight
                       (stream-map (lambda (x) (list (stream-first s0) x)) (stream-rest s1))
                       (weighted-pairs weight (stream-rest s0) (stream-rest s1)))))
  )

(define (stream-i+j max-n)
  (weighted-pairs (lambda (pair) (+ (car pair) (cadr pair))) (in-range 1 max-n) (in-range 1 max-n))
  )
(define (stream-2i+3j+5ij max-n)
  (define (div n k) (= 0 (remainder n k)))
  (define list-2-3-5 (list 2 3 5))
  (define (div-2-3-5 n)
    (ormap (lambda (i) (div n i)) list-2-3-5)
    )
  (stream-filter (lambda (pair) (ormap div-2-3-5 pair))
                 (weighted-pairs (lambda (pair) (+ (* 2 (car pair)) (* 3 (cadr pair)) (* 5 (car pair) (cadr pair)))) 
                                 (in-range 1 max-n) 
                                 (in-range 1 max-n)))
  )

(stream->list (stream-i+j 10))
(stream->list (stream-2i+3j+5ij 10))
