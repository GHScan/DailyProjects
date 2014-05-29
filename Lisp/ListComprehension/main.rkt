#lang racket

;------------------------------
(define (flatten-map f l)
  (apply append (map f l))
  )
(define-syntax list-of
  (syntax-rules (<-)
    [(list-of exp (x <- x-list) rule ...) (flatten-map (lambda (x) (list-of exp rule ...)) x-list)]
    [(list-of exp filter rule ...) (if filter (list-of exp rule ...) empty)]
    [(list-of exp) (list exp)])
  )
;------------------------------
(require math/number-theory)

(list-of (list x y (+ x y)) (x <- (range 1 10)) (y <- (range 1 10)) (prime? (+ x y)))
