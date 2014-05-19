#lang racket

(require "25.rkt")

;------------------------------
(eval G
      '(begin
         (define raw-pair? pair?)
         (define raw-cons cons)
         (define raw-car car)
         (define raw-cdr cdr)
         (define raw-pretty-print pretty-print)
         (define defered-pair-tag (list 'defered-pair))
         (define (cons a b)
           (raw-cons defered-pair-tag (lambda (first) (if first a b)))
           )
         (define (car pair)
           ((raw-cdr pair) true)
           )
         (define (cdr pair)
           ((raw-cdr pair) false)
           )
         (define (pair? v)
           (and (raw-pair? v) (eq? (raw-car v) defered-pair-tag))
           )
         (define (list . args)
           (raw-list->list args)
           )
         (define (length l)
           (if (null? l)
             0
             (+ 1 (length (cdr l))))
           )
         (define (append a b)
           (if (null? a) 
             b
             (cons (car a) (append (cdr a) b)))
           )
         (define (pretty-print a)
           (cond
             ((null? a) (raw-pretty-print a))
             ((pair? a) (raw-pretty-print (list->raw-list a)))
             (else (raw-pretty-print a)))
           )
         (define (raw-list->list raw-list)
           (if (null? raw-list)
             empty
             (cons (raw-car raw-list) (raw-list->list (raw-cdr raw-list))))
           )
         (define (list->raw-list l)
           (if (null? l)
             empty
             (raw-cons (car l) (list->raw-list (cdr l))))
           )
         (define (curry n f . _boundArgs)
           (lambda _args 
             (let ((boundArgs (raw-list->list _boundArgs))(args (raw-list->list _args)))
               (let ((all-args (append boundArgs args)))
                 (if (>= (length all-args) n)
                   (apply f (list->raw-list all-args))
                   (apply curry n f (list->raw-list all-args))))))
           )
         ))

;------------------------------
(eval G
      '(pretty-print (map (lambda (i) (* i i)) 
                          (filter (lambda (i) (= 0 (remainder i 2))) 
                                  (build-list 10 identity))))
      )

(eval G 
      '(begin
         (define (fib n)
           (if (<= n 1)
             1
             (+ (fib (- n 1)) (fib (- n 2))))
           )
         (pretty-print (build-list 10 fib))))

(eval G
      '(let ((x 1))
         (let ((y 2))
           (set! x (+ y 1))
           (set! y (+ x 1))
           (pretty-print (or (> x y) (cons (+ x y) (list 1 2 3)))))) )

(eval G 
      '(pretty-print (map (curry 2 * 2) (build-list 10 identity))))

(eval G 
      '(pretty-print (eval! '(foldl (curry 3 + 1) 0 (build-list 10 identity)))))

;------------------------------
(eval G
      '(begin
         (define (list-ref items n)
           (if (= n 0)
             (car items)
             (list-ref (cdr items) (- n 1)))
           )
         (define (add-lists a b)
           (if (null? a)
             empty
             (cons (+ (car a) (car b)) (add-lists (cdr a) (cdr b))))
           )

         (define ones (cons 1 ones))
         (define integers (cons 1 (add-lists integers ones)))
         (pretty-print (list-ref integers 17))
         ))

(eval G 
      '(begin
         (define (integral integrand init-value dt)
           (define init (cons init-value (add-lists init (map (curry 2 * dt) integrand))))
           init)
         (define (solve f y0 dt)
           (define y (integral dy y0 dt))
           (define dy (map f y))
           y)
         (pretty-print (list-ref (solve identity 1 0.001) 1000))
         )
      )

;------------------------------
; exercise 32

(eval G
 '(begin
     (define print-ints (map (lambda (x) (printf "{~a}\n" x) x) integers))
     (list-ref print-ints 11)
     ))
