#lang racket

(require "66.rkt")

(define (triples s0 s1 s2)
  (if (or (stream-empty? s0) (stream-empty? s1) (stream-empty? s2))
    empty-stream
    (let ((s0-car (stream-first s0))(s0-cdr (stream-rest s0))
          (s1-car (stream-first s1))(s1-cdr (stream-rest s1))
          (s2-car (stream-first s2))(s2-cdr (stream-rest s2)))
      (stream-cons (list s0-car s1-car s2-car)
                   (interleave (stream-map (lambda (x) (list s0-car s1-car x)) s2-cdr)
                               (interleave (stream-map (lambda (pair) (cons s0-car pair)) (pairs s1-cdr s2-cdr))
                                           (triples s0-cdr s1-cdr s2-cdr)))
                   )))
  )

(define (pythagoras-triples max-n)
  (define integers (in-range 1 max-n))
  (stream-filter 
    (lambda (triple) (= (+ (sqr (list-ref triple 0)) (sqr (list-ref triple 1))) (sqr (list-ref triple 2)))) 
    (triples integers integers integers))
  )

(stream->list (pythagoras-triples 100))
