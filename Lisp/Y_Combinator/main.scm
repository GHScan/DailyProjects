#lang racket
(require racket/pretty)

;******************************
; 1. usefull implemention, but is not real Y-combinator
(define Y1
 (lambda (f)
  (lambda args (apply f (Y1 f) args))
  ))

; 2. use a amazing skill to remove the reference of procedure name
(define Y2
 (lambda (f)
  ((lambda (g) (g g f))
   (lambda (g f)
    (lambda args (apply f (g g f) args))
    ))))

; 3. for Y-combinator usage, the parameter of g (the f) will not change, so it can be removed
(define Y3
 (lambda (f)
  ((lambda (g) (g g))
   (lambda (g)
    (lambda args (apply f (g g) args))))))

; test
(define Y Y3)
(define fib
  (Y 
    (lambda (self x)
      (if (<= x 1)
        1
        (+ (self (- x 1)) (self (- x 2))))
      )))
(define fac
  (Y
    (lambda (self x)
      (if (<= x 1)
        1
        (* x (self (- x 1))))
      )))

(pretty-print (build-list 10 fib))
(pretty-print (build-list 10 fac))

;******************************
; demonstrate the amazing skill mentioned above use to remove procedure name
(define fib2
  (lambda (x)
    (if (<= x 1)
      1 
      (+ (fib2 (- x 1)) (fib2 (- x 2)))))
  )
(pretty-print (build-list 10 fib2))

(define fib2-without-name
  (lambda (x)
    ((lambda (g) (g g x))
     (lambda (g x)
       (if (<= x 1)
         1
         (+ (g g (- x 1)) (g g (- x 2)))))))
  )
(pretty-print (build-list 10 fib2-without-name))

;******************************
; another style of Y-combinator
(define Y4
 (lambda (f)
  ((lambda (g) (g g))
   (lambda (g)
    (f (lambda args (apply (g g) args))))))
 )

; test
(define YY Y4)
(define fib3
  (YY
    (lambda (self)
      (lambda (x)
        (if (<= x 1)
        1
        (+ (self (- x 1)) (self (- x 2)))))))
  )
(define fac3
  (YY
    (lambda (self)
      (lambda (x)
        (if (<= x 1)
          1
          (* x (self (- x 1)))))))
  )
(pretty-print (build-list 10 fib3))
(pretty-print (build-list 10 fac3))
