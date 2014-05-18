#lang racket

(provide (all-defined-out))
;------------------------------
(define (make-env prev-env)
  (cons (make-hasheq) prev-env)
  )

(define (env-lookup env name)
  (cond 
    ((null? env) (error "Can't find variable! " name))
    ((hash-has-key? (car env) name) (hash-ref (car env) name))
    (else (env-lookup (cdr env) name)))
  )

(define (env-define-variable! env name var)
  (hash-set! (car env) name var)
  )

(define (env-set-variable! env name var)
  (cond 
    ((null? env) (error "Can't find variable! " name))
    ((hash-has-key? (car env) name) (hash-set! (car env) name var))
    (else (env-set-variable! (cdr env) name var)))
  )

(define (env-undefine-variable! env name)
  (hash-remove! (car env) name)
  )
;------------------------------
(define (exp-literal? exp)
  (or (string? exp) (number? exp))
  )

(define (exp-variable? exp)
  (symbol? exp)
  )

(define (exp-tag exp)
  (car exp)
  )
;------------------------------
(define (script-procedure? p)
  (and (list? p) (eq? (car p) 'script-procedure))
  )

(define (make-script-procedure env args body)
  (list 'script-procedure env args body)
  )

(define (bind-script-procedure-args env p-args args)
  (cond
    ((symbol? p-args) (env-define-variable! env p-args args))
    ((null? p-args) (if (null? args) 'ok (error "arg count mismatch!")))
    (else (env-define-variable! env (car p-args) (car args)) 
          (bind-script-procedure-args env (cdr p-args) (cdr args))))
  )

(define (eval-script-procedure p args)
  (let ((p-env (cadr p))(p-args (caddr p))(p-body (cadddr p)))
    (let ((new-env (make-env p-env)))
      (bind-script-procedure-args new-env p-args args)
      (eval-begin new-env p-body)))
  )

;------------------------------
(define (eval-literal env exp) 
  exp
  )

(define (eval-variable env exp) 
  (env-lookup env exp)
  )

(define (eval-procedure p args)
 (if (script-procedure? p)
   (eval-script-procedure p args)
   (apply p args))
 )
;------------------------------
(define (eval-if env exp)
  (let ((pred-exp (car exp))(then-exp (cadr exp))(else-exp (caddr exp)))
    (if (eval env pred-exp)
      (eval env then-exp)
      (eval env else-exp)))
  )

(define (eval-lambda env exp)
  (make-script-procedure env (car exp) (cdr exp))
  )

(define (eval-set! env exp)
  (env-set-variable! env (car exp) (eval env (cadr exp)))
  )

(define (eval-and env exp)
  (andmap (curry eval env) exp)
  )

(define (eval-or env exp)
  (ormap (curry eval env) exp)
  )

(define (eval-quote env exp)
  (car exp)
  )

(define (eval-begin env exp)
  (if (null? (cdr exp))
    (eval env (car exp))
    (begin (eval env (car exp)) (eval-begin env (cdr exp))))
  )

(define (eval-cond env exp)
  (let ((case-pred (caar exp))(case-body (cdar exp))(rest (cdr exp)))
    (if (null? rest)
      (eval-begin env case-body)
      (if (eval env case-pred)
        (eval-begin env case-body)
        (eval-cond env rest))))
  )

(define (eval-define env exp)
  (if (symbol? (car exp))
    (env-define-variable! env (car exp) (eval env (cadr exp)))
    (env-define-variable! env (caar exp) 
                          (eval-lambda env (cons (cdar exp) (cdr exp)))))
  )

(define (eval-let env exp)
  (let ((names (map car (car exp)))(vars (map cadr (car exp)))(body (cdr exp)))
    (eval-script-procedure (eval-lambda env (cons names body)) (map (curry eval env) vars)))
  )

(define (eval-eval! env exp)
  (eval env (eval env (car exp)))
  )
;------------------------------
(define the-special-forms (make-hasheq))

(define (special-form-add keyword procedure)
  (hash-set! the-special-forms keyword procedure)
  )

(define (special-form-lookup keyword)
  (hash-ref the-special-forms keyword false)
  )
;------------------------------
(define G (make-env empty))

(define (add-builtin name val)
  (env-define-variable! G name val)
  )
;------------------------------
(define (script-apply p . args)
  (define (flatten-variadic-args args)
    (cond
      ((null? args) empty)
      ((null? (cdr args)) (if (list? (car args)) (car args) (list (car args))))
      (else (cons (car args) (flatten-variadic-args (cdr args)))))
    )
  (eval-procedure p (flatten-variadic-args args))
  )

(define (register-builtin-script-procedures)
  (eval G 
        '(begin
           (define (build-list n proc)
             (define (iter i result)
               (if (= 0 i)
                 result
                 (iter (- i 1) (cons (proc (- i 1)) result)))
               )
             (iter n empty)
             )
           (define (foldl proc init l)
             (if (null? l)
               init
               (foldl proc (proc (car l) init) (cdr l)))
             )
           (define (map proc l)
             (if (null? l)
               empty
               (cons (proc (car l)) (map proc (cdr l))))
             )
           (define (filter pred l)
             (cond
               ((null? l) empty)
               ((pred (car l)) (cons (car l) (filter pred (cdr l))))
               (else (filter pred (cdr l))))
             )
           (define (curry n f . boundArgs)
             (lambda args 
               (let ((all-args (append boundArgs args)))
                 (if (>= (length all-args) n)
                   (apply f all-args)
                   (apply curry n f all-args))))
             )
           )
        )
  )

(define (setup)
  (let ((builtin-procedures (list 
                              (cons '+ +)
                              (cons '- -)
                              (cons '* *)
                              (cons '/ /)
                              (cons '< <)
                              (cons '<= <=)
                              (cons '> >)
                              (cons '>= >=)
                              (cons '= =)
                              (cons 'not not)
                              (cons 'quotient quotient)
                              (cons 'remainder remainder)
                              (cons 'cons cons)
                              (cons 'car car)
                              (cons 'cdr cdr)
                              (cons 'null? null?)
                              (cons 'empty empty)
                              (cons 'list list)
                              (cons 'true true)
                              (cons 'false false)
                              (cons 'else true)
                              (cons 'identity identity)
                              (cons 'length length)
                              (cons 'append append)
                              (cons 'apply script-apply)
                              (cons 'pretty-print pretty-print)
                              (cons 'current-inexact-milliseconds current-inexact-milliseconds)
                              (cons 'printf printf)
                              (cons 'range range)
                              ))
        (spectial-forms (list 
                          (cons 'if eval-if)
                          (cons 'cond eval-cond)
                          (cons 'define eval-define)
                          (cons 'lambda eval-lambda)
                          (cons 'and eval-and)
                          (cons 'or eval-or)
                          (cons 'let eval-let)
                          (cons 'quote eval-quote)
                          (cons 'begin eval-begin)
                          (cons 'set! eval-set!)
                          (cons 'eval! eval-eval!)
                          )))
    (for-each (lambda (p) (add-builtin (car p) (cdr p))) builtin-procedures)
    (for-each (lambda (p) (special-form-add (car p) (cdr p))) spectial-forms))
    (register-builtin-script-procedures)
  )

(define (eval env exp)
  (cond 
    ((exp-literal? exp) (eval-literal env exp))
    ((exp-variable? exp) (eval-variable env exp))
    ((special-form-lookup (exp-tag exp)) => (lambda (p) (p env (cdr exp))))
    (else (let ((p (eval env (car exp)))(args (map (lambda (e) (eval env e)) (cdr exp))))
            (eval-procedure p args))))
  )

;------------------------------
(define (run-test)
  (pretty-print 
    (eval G 
          '(map (lambda (i) (* i i)) 
                (filter (lambda (i) (= 0 (remainder i 2))) 
                        (build-list 10 identity)))
          ))

  (pretty-print 
    (eval G 
          '(begin
             (define (fib n)
               (if (<= n 1)
                 1
                 (+ (fib (- n 1)) (fib (- n 2))))
               )
             (build-list 10 fib))))

  (pretty-print 
    (eval G
          '(let ((x 1))
             (let ((y 2))
               (set! x (+ y 1))
               (set! y (+ x 1))
               (or (> x y) (cons (cons x y) '(1 2 3))))) ))

  (pretty-print 
    (eval G 
          '(map (curry 2 * 2) (build-list 10 identity))))

  (pretty-print 
   (eval G 
    '(eval! '(foldl (curry 3 + 1) 0 (build-list 10 identity)))))
  )

(define (benchmark)
  (eval G
        '(define (timing f loop)
           (define (iter n)
             (if (= 1 n)
               (f)
               (begin (f) (iter (- n 1))))
             )
           (define start (current-inexact-milliseconds))
           (iter loop)
           (printf "~a\n" (- (current-inexact-milliseconds) start))
           ))
  (eval G
        '(define (fib n)
           (define (iter n a b)
             (if (= 0 n)
               a
               (iter (- n 1) b (+ a b)))
             )
           (iter n 0 1)
           ))
  (eval G
   '(begin 
       (timing (lambda () (fib 30)) 5000)
       (timing (lambda () (map (curry 2 * 2) (range 256))) 300)
       ))
  )

;------------------------------
(setup)
;(run-test)
;(benchmark)
