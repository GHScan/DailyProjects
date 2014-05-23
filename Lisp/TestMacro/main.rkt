#lang racket

;------------------------------
(define (lazy-obj f) 
  (let ((value false))
    (lambda () 
      (if value
        value
        (begin (set! value (f))
               value))))
  )

;------------------------------
(define-syntax my-cond
  (syntax-rules (else)
    ((my-cond (else exp1 ...)) (begin exp1 ...))
    ((my-cond (pred1 ...)) (begin pred1 ...))
    ((my-cond (pred exp1 ...) c1 ...) (if pred (begin exp1 ...) (my-cond c1 ...)))
    )
  )
(define-syntax my-and
  (syntax-rules ()
    ((my-and) #t)
    ((my-and e) e)
    ((my-and e1 e2 ...) (if e1 (my-and e2 ...) #f))
    )
  )
(define-syntax my-let 
  (syntax-rules ()
    ((my-let ((name value) ...) exp1 ...) ((lambda (name ...) exp1 ...) value ...))
    )
  )
(define-syntax my-cons
  (syntax-rules ()
    ((my-cons a b) (cons (lazy-obj (lambda () a)) (lazy-obj (lambda () b))))
    )
  )
(define (my-car p)
  ((car p))
  )
(define (my-cdr p)
  ((cdr p))
  )

;------------------------------
(define (my-map proc . streams)
  (if (empty? (car streams))
    empty
    (my-cons (apply proc (map my-car streams))
             (apply my-map proc (map my-cdr streams))))
  )
(define (my-ref s n)
  (if (zero? n)
    (my-car s)
    (my-ref (my-cdr s) (- n 1)))
  )

;------------------------------
(define (f n)
  (my-cond
    ((< n 5) "less than 5")
    ((< n 10) "less than 10")
    (else "greater than 10")
    )
  )
(build-list 12 f)
(my-and #f 4)
(my-let ((a 3)(b 2))
        (set! a (* a a))
        (+ a b))
(define ones (my-cons 1 ones))
(define ints (my-cons 1 (my-map + ones ints)))
(define fib (my-cons 1 (my-cons 1 (my-map + fib (my-cdr fib)))))
(build-list 10 (curry my-ref ones))
(build-list 10 (curry my-ref ints))
(build-list 40 (curry my-ref fib))
