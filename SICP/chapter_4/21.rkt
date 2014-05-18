#lang racket

(require "01.rkt")

;------------------------------
; factorial
(eval G
      '((lambda (n)
          ((lambda (f) (f f n))
           (lambda (self a)
             (if (= a 1)
               a
               (* a (self self (- a 1)))))))
        10)
      )

; Y combinator
(eval G
      '(((lambda (f)
           ((lambda (Y) (Y Y))
            (lambda (Y-self)
              (f (lambda args (apply (Y-self Y-self) args))))))
         (lambda (self)
           (lambda (a)
             (if (= 1 a)
               a
               (* a (self (- a 1)))))))
        10))

;------------------------------
; fibonacci

(eval G
      '(build-list
         10
         (lambda (n)
           ((lambda (f) (f f 0 1 n))
            (lambda (self a b n)
              (if (= n 0)
                a
                (self self b (+ a b) (- n 1))))))))

; Y combiantor
(eval G
      '(build-list 
         10
         (lambda (n)
           (((lambda (f)
               ((lambda (Y) (Y Y))
                (lambda (Y-self)
                  (f (lambda args (apply (Y-self Y-self) args))))))
             (lambda (self)
               (lambda (a b n)
                 (if (= n 0)
                   a
                   (self b (+ a b) (- n 1))))))
            0 1 n))))

;------------------------------
; f 

(eval G
      '(build-list 
         10
         (lambda (n)
           ((lambda (even? odd?) (even? even? odd? n))
            (lambda (even? odd? n)
              (if (= 0 n)
                true
                (odd? even? odd? (- n 1))))
            (lambda (even? odd? n) 
              (if (= 0 n)
                false
                (even? even? odd? (- n 1))))))))
