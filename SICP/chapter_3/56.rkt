#lang racket

(define (stream-scale x s)
  (if (stream-empty? s)
    s
    (stream-cons (* x (stream-first s)) (stream-scale x (stream-rest s))))
  )

(define (stream-merge less? s1 s2)
  (cond
    ((stream-empty? s1) s2)
    ((stream-empty? s2) s1)
    ((less? (stream-first s1) (stream-first s2)) 
     (stream-cons (stream-first s1) (stream-merge less? (stream-rest s1) s2)))
    ((less? (stream-first s2) (stream-first s1)) 
     (stream-cons (stream-first s2) (stream-merge less? s1 (stream-rest s2))))
    (else 
      (stream-cons (stream-first s1) (stream-merge less? (stream-rest s1) (stream-rest s2))))
    )
  )

(define S (stream-cons 1 (stream-merge < S2 (stream-merge < S3 S5))))
(define S2 (stream-scale 2 S))
(define S3 (stream-scale 3 S))
(define S5 (stream-scale 5 S))

(build-list 32 (lambda (i) (stream-ref S i)))
