#lang racket

(require "81.rkt")

(define (stream-map proc . streams)
  (if (stream-empty? (car streams))
    empty-stream
    (stream-cons (apply proc (map stream-first streams))
                 (apply stream-map proc (map stream-rest streams)))
    )
  )

(define (mente-carlo s)
  (define stream-01 (stream-map (lambda (b) (if b 1.0 0)) s))
  (define sum (stream-cons (stream-first stream-01) (stream-map + (stream-rest stream-01) sum)))
  (stream-map / sum (in-naturals 1))
  )

(define (make-random-2-stream randoms)
  (stream-cons (list (stream-ref randoms 0) (stream-ref randoms 1))
               (make-random-2-stream (stream-rest (stream-rest randoms))))
  )

(define (random-in-range r x1 x2)
  (+ x1 (* (/ r 65536.0) (- x2 x1)))
  )

(define (make-y-fx-stream P x1 x2 y1 y2)
  (stream-map 
    (lambda (r2) (P (random-in-range (car r2) x1 x2) (random-in-range (cadr r2) y1 y2)))
    (make-random-2-stream (make-random-stream (make-cycle-stream (stream (list 'generate))) 12345)))
  )

(define (estimate-integral P x1 x2 y1 y2)
  (stream-map (curry * (- x2 x1) (- y2 y1)) (mente-carlo (make-y-fx-stream P x1 x2 y1 y2)))
  )

(define pi (estimate-integral (lambda (x y) (<= (+ (sqr x) (sqr y)) 1)) -1 1 -1 1))
(stream-ref pi 256)
