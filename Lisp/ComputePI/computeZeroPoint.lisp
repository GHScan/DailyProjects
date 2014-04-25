#lang racket
(require racket/pretty)

(define (float-equal? a b)
  (< (abs (- a b)) 0.001)
  )

(define (search f a b)
  (let ((mid (/ (+ a b) 2.0)))
    (cond
      ((float-equal? a b) mid)
      ((< (f mid) 0) (search f mid b))
      ((> (f mid) 0) (search f a mid))
      (else mid)))
  )

(define (half-interval-method f a b)
  (let ((av (f a))
        (bv (f b)))
    (cond
      ((and (<= av 0) (>= bv 0)) (search f a b))
      ((and (>= av 0) (<= bv 0)) (search f b a))
      (else (error "invalid range" a b))))
  )

(define mypi (half-interval-method sin 2 4))

(pretty-print mypi)
