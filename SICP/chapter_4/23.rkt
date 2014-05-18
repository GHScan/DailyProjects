#lang racket

(define (timing f loop)
  (define (iter n)
    (if (= 1 n)
      (f)
      (begin (f) (iter (- n 1))))
    )
  (let ((start-time (current-inexact-milliseconds)))
    (iter loop)
    (pretty-print (- (current-inexact-milliseconds) start-time)))
  )

(define ops (build-list 16 (lambda (i)
                             (cond 
                               ((= 0 (remainder i 6)) add1)
                               ((= 1 (remainder i 6)) sub1)
                               ((= 2 (remainder i 6)) (lambda (i) (+ i 2)))
                               ((= 3 (remainder i 6)) (lambda (i) (- i 2)))
                               ((= 4 (remainder i 6)) (lambda (i) (+ i 3)))
                               (else (lambda (i) (* i 1)))))))

(define (compile-2 op ops)
  (if (null? ops) 
   op
   (compile-2 (lambda (n) ((car ops) (op n))) (cdr ops)))
 )

; compile-3 generate tail recursion procedure, it's respected to run fast, but it doesn't...
; but in lua, compile-3 will be faster than compile-2
(define (compile-3 op ops)
  (if (null? ops)
    op
    (let ((post (compile-3 (car ops) (cdr ops))))
      (lambda (n) (post (op n)))))
  )

(define ops-compile1 (lambda (n) (foldl (lambda (f init) (f init)) n ops)))
(define ops-compile2 (compile-2 (car ops) (cdr ops)))
(define ops-compile3 (compile-3 (car ops) (cdr ops)))

(ops-compile1 10)
(ops-compile2 10)
(ops-compile3 10)

(timing (lambda () (ops-compile1 10)) 1000000)
(timing (lambda () (ops-compile2 10)) 1000000)
(timing (lambda () (ops-compile3 10)) 1000000)
