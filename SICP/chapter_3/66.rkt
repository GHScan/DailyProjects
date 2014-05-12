#lang racket

(provide (all-defined-out))
;------------------------------
(define (stream-map proc . streams)
  (if (stream-empty? (car streams))
    empty-stream
    (stream-cons (apply proc (map stream-first streams))
                 (apply stream-map proc (map stream-rest streams)))
    )
  )

(define ones (stream-cons 1 ones))
(define integers (stream-cons 1 (stream-map + integers ones)))

(define (interleave s0 s1)
  (if (stream-empty? s0) 
    s1
    (stream-cons (stream-first s0) (interleave s1 (stream-rest s0))))
  )

(define (pairs s0 s1)
  (if (or (stream-empty? s0) (stream-empty? s1))
    empty-stream
    (stream-cons (list (stream-first s0) (stream-first s1))
                 (interleave (stream-map (lambda (x) (list (stream-first s0) x)) (stream-rest s1))
                             (pairs (stream-rest s0) (stream-rest s1))))
    )
  )

(define (checked-stream f s)
  (stream-map f integers s)
  )
;------------------------------
(define (pair->n i j)
  (let ((step0 (expt 2 (- i 2)))(step1 (expt 2 (- i 1)))(step2 (expt 2 i)))
    (cond 
      ((and (= i 1) (= j 1)) 1)
      ((= i j) (+ (pair->n (- i 1) j) step0))
      (else (+ (pair->n i i) step1 (* (- j (+ i 1)) step2)))
      ))
  )
(define (check-int-pair n pair)
  (= n (pair->n (list-ref pair 0) (list-ref pair 1)))
  )

(define int-pairs (pairs integers integers))
(define checks (checked-stream check-int-pair int-pairs))

(build-list 48 (lambda (i) (stream-ref int-pairs i)))
(build-list 48 (lambda (i) (stream-ref checks i)))
