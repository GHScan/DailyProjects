#lang racket

(require "utils.rkt")

(define (binary-palindrome-list len)
  (cond
    [(= len 1) '((0) (1))]
    [(= len 2) '((0 0) (1 1))]
    [else 
      (let ([rest (binary-palindrome-list (- len 2))])
        (flatten-map 
          (lambda (sub)
            `((0 ,@sub 0)(1 ,@sub 1)))
          rest))])
  )

(define (binary->number l)
  (foldl (lambda (b init) (+ b (* 2 init))) 0 l)
  )

;------------------------------
(define binarys 
  (apply append
         (map
           (lambda (len)
             (filter (lambda (binary) (= 1 (car binary))) (binary-palindrome-list len)))
           (range 1 21))))

(define result 
 (filter (lambda (n) (palindrome? (number->list n))) (map binary->number binarys)))
(apply + result)
