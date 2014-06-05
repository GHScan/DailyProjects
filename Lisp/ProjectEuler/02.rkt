#lang racket

(require "utils.rkt")
(define fibs (stream-cons 1 (stream-cons 2 (stream-map + fibs (stream-rest fibs)))))

;(stream->list (stream-take fibs 10))
(foldl + 0 (filter even? (stream->list (stream-take-until fibs (lambda (n) (>= n 4000000))))))
