#lang racket

(require "utils.rkt")

(define table (make-prime-table 20000))

(define (find-max-n a b n)
  (let ([r (+ (* n n) (* n a) b)])
    (if (and (> r 0) (vector-ref table r))
      (find-max-n a b (+ n 1))
      n))
  )

(define max-n 0)
(define max-ab empty)

(do ([a -999 (+ a 1)])
  ((> a 999))
  (do ([b -999 (+ b 1)])
    ((> b 999))
    (let ([n (find-max-n a b 0)])
      (if (> n max-n)
        (begin (set! max-n n)
               (set! max-ab (list a b)))
        'ok)))
  )

(apply * max-ab)
