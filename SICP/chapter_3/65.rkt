#lang racket

;------------------------------
(define (stream-map proc . streams)
  (if (stream-empty? (car streams))
    empty-stream
    (stream-cons (apply proc (map stream-first streams))
                 (apply stream-map proc (map stream-rest streams)))
    )
  )

(define (partial-sums s)
  (define news (stream-cons (stream-first s) (stream-map + (stream-rest s) news)))
  news)

(define (stream-take n s)
  (build-list n (lambda (i) (stream-ref s i)))
  )

(define (euler-transform s)
  (let ((s0 (stream-ref s 0))(s1 (stream-ref s 1))(s2 (stream-ref s 2)))
    (stream-cons (- s2 (/ (sqr (- s2 s1)) (+ s2 s0 (* -2 s1))))
                 (euler-transform (stream-rest s))))
  )

(define (make-tableau transform s)
  (stream-cons s (make-tableau transform (transform s)))
  )

(define (accelerated-sequence transform s)
  (stream-map stream-first (make-tableau transform s))
  )
;------------------------------
(define ln2-stream
  (partial-sums 
    ((lambda ()
       (define (iter n)
         (stream-cons (/ 1 n)
                      (stream-map - (iter (+ n 1))))
         )
       (iter 1.0))))
  )

(stream-take 8 ln2-stream)
(stream-take 8 (euler-transform ln2-stream))
(stream-take 8 (accelerated-sequence euler-transform ln2-stream))
