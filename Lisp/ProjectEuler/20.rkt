#lang racket

(define (solve n)
  (apply 
    +
    (map 
      (lambda (c) (- (char->integer c) (char->integer #\0)))
      (string->list 
        (number->string (foldl * 1 (range 1 (+ 1 n)))))))
  )

(solve 10)
(solve 100)
