#lang racket

(require racket/match)
(require compatibility/mlist)

;------------------------------
; primitive procedures
(define G (mcons 'env
                 (mlist 
                   (mcons '+ +)
                   (mcons '- -)
                   (mcons '* *)
                   (mcons '/ /)
                   (mcons 'quotient quotient)
                   (mcons 'remainder remainder)
                   (mcons 'true true)
                   (mcons 'false false)
                   (mcons 'else true)
                   (mcons '= =)
                   (mcons 'not not)
                   (mcons '< <)
                   (mcons '<= <=)
                   (mcons '> >)
                   (mcons '>= >=)
                   (mcons 'cons cons)
                   (mcons 'car car)
                   (mcons 'cdr cdr)
                   (mcons 'empty empty)
                   (mcons 'empty? empty?)
                   (mcons 'length length)
                   (mcons 'append append)
                   (mcons 'printf printf)
                   (mcons 'print print)
                   (mcons 'pretty-print pretty-print)
                   (mcons 'identity identity)
                   (mcons 'build-list build-list)
                   (mcons 'map map)
                   (mcons 'filter filter)
                   (mcons 'foldl foldl)
                   (mcons 'curry curry)
                   (mcons 'range range)
                   (mcons 'current-inexact-milliseconds current-inexact-milliseconds)
                   (mcons 'eval (lambda (e) (eval G e)))
                   )))

;------------------------------
; the interpreter
(define (make-env env vars values)
  (mcons 'env
         (let iterate ([pairs (mcdr env)][vars vars][values values])
           (cond
             [(empty? vars) pairs]
             [(symbol? vars) (mcons (vars values) pairs)]
             [else (iterate (mcons (mcons (car vars) (car values)) pairs) (cdr vars) (cdr values))])))
  )

(define (eval env exp)
  (cond
    [(or (string? exp) (number? exp)) exp]
    [(symbol? exp) (mcdr (massq exp (mcdr env)))]
    [else 
      (match 
        exp
        [(list 'quote e) e]
        [(list 'if pred then else) (if (eval env pred) (eval env then) (eval env else))]
        [(list 'begin exps ...) (foldl (lambda (e _) (eval env e)) empty exps)]
        [(list 'lambda p-args p-bodys ...) 
         (lambda args (eval (make-env env p-args args) (cons 'begin p-bodys)))]
        [(list 'define (list p args ...) bodys ...) (eval env `(define ,p (lambda ,args ,@bodys)))]
        [(list 'define var e) (set-mcdr! env (mcons (mcons var (eval env e)) (mcdr env)))]
        [(list 'set! var e) (set-mcdr! (massq var (mcdr env)) (eval env e))]
        [(list 'cond (list case-preds case-bodys ...) ...) 
         (do ([pred case-preds (cdr pred)][body case-bodys (cdr body)])
           ((eval env (car pred)) (eval env (cons 'begin (car body)))))]
        [(list 'and exps ...) (andmap (curry eval env) exps)]
        [(list 'or exps ...) (ormap (curry eval env) exps)]
        [(list 'let (list (list vars values) ...) bodys ...) (eval env `((lambda ,vars ,@bodys) ,@values))]
        [(list 'letrec (list (list vars values) ...) bodys ...) 
         (let ([new-env (make-env env vars (map (lambda (_) 'undefined) vars))])
           (foldl (lambda (var v _) (eval new-env `(set! ,var ,v))) empty vars values)
           (eval new-env (cons 'begin bodys)))]
        [(list p args ...) (apply (eval env p) (map (lambda (e) (eval env e)) args))])])
  )

;------------------------------
; testing

(eval G '(begin
           (pretty-print 
             (letrec ([fib 
                        (lambda (a b n)
                          (cond 
                            [(= n 0) a]
                            [else (fib b (+ a b) (- n 1))]))])
               (map (curry fib 0 1) (range 20))))
           (pretty-print 
             (map (lambda (i) (* i i)) 
                  (filter (lambda (i) (= 0 (remainder i 2))) 
                          (build-list 10 identity))))
           (pretty-print 
             (let ((x 1))
               (let ((y 2))
                 (set! x (+ y 1))
                 (set! y (+ x 1))
                 (or (> x y) (cons (cons x y) '(1 2 3))))))
           (pretty-print 
             (map (curry * 2) (build-list 10 identity)))
           (pretty-print 
             (eval '(foldl (curry + 1) 0 (build-list 10 identity))))
           ))

; benchmark
(eval G
      '(begin
         (define (timing f loop)
           (define (iter n)
             (if (= 1 n)
               (f)
               (begin (f) (iter (- n 1))))
             )
           (define start (current-inexact-milliseconds))
           (iter loop)
           (printf "~a\n" (- (current-inexact-milliseconds) start))
           )
         (define (fib n)
           (define (iter n a b)
             (if (= 0 n)
               a
               (iter (- n 1) b (+ a b)))
             )
           (iter n 0 1)
           )
         (timing (lambda () (fib 30)) 5000)
         (timing (lambda () (map (curry * 2) (range 256))) 300)
         ))

