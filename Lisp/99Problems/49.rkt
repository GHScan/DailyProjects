#lang racket

(define (gray n)
  (if (= 0 n)
    (list empty)
    (let ([rest (gray (- n 1))])
      (append
        (map (lambda (sub) (cons 0 sub)) rest)
        (map (lambda (sub) (cons 1 sub)) rest))))
  )

;------------------------------
(gray 2)
(gray 3)
(gray 4)
