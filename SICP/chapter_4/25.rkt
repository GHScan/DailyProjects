#lang racket

;------------------------------
(define thunk-tag (list 'thunk))
(define forced-thunk-tag (list 'forced-thunk))
(define (make-thunk env code)
  (mcons thunk-tag (lambda () (force-value (code env))))
  )
(define (thunk? value)
  (and (mpair? value) (or (eq? forced-thunk-tag (mcar value)) (eq? thunk-tag (mcar value))))
  )
(define (thunk-value thunk)
  (if (eq? forced-thunk-tag (mcar thunk))
    (mcdr thunk)
    (begin (set-mcar! thunk forced-thunk-tag)
           (set-mcdr! thunk ((mcdr thunk)))
           (mcdr thunk)))
  )

(define (force-value value)
  (if (thunk? value)
    (thunk-value value)
    value)
  )
;------------------------------
(define (make-env prev-env)
  (cons (make-hasheq) prev-env)
  )

(define (env-define-variable! env name var)
  (hash-set! (car env) name var)
  )

(define (env-get-variable-depth env name)
  (define (iter env name depth)
    (if (hash-has-key? (car env) name)
      depth
      (iter (cdr env) name (add1 depth)))
    )
  (iter env name 0)
  )

(define (env-lookup-variable env depth name)
  (if (zero? depth)
    (hash-ref (car env) name false)
    (env-lookup-variable (cdr env) (sub1 depth) name))
  )

(define (env-set-variable! env depth name var)
  (if (zero? depth)
    (hash-set! (car env) name var)
    (env-set-variable! (cdr env) (sub1 depth) name var))
  )
;------------------------------
(define (make-script-procedure env args body)
  (list env args body)
  )

(define (setup-call-env env p-args args)
  (cond
    ((symbol? p-args) (env-define-variable! env p-args args) env)
    ((null? p-args) (if (null? args) env (error "argument count mismatch!")))
    (else (env-define-variable! env (car p-args) (car args)) 
          (setup-call-env env (cdr p-args) (cdr args)))
    )
  )

(define (call-native-or-script-procedure p args)
  (if (procedure? p)
    (apply p args)
    (let ((p-env (car p))(p-args (cadr p))(p-body (caddr p)))
      (p-body (setup-call-env (make-env p-env) p-args args))))
  )
;------------------------------
(define (exp-literal? exp)
  (or (string? exp) (number? exp))
  )
(define (compile-literal exp)
  (lambda (env) exp)
  )

(define (exp-variable? exp)
  (symbol? exp)
  )
(define (compile-variable exp)
  (let ((depth false))
    (lambda (env) 
      (if depth
        (env-lookup-variable env depth exp)
        (begin (set! depth (env-get-variable-depth env exp)) 
               (env-lookup-variable env depth exp)))
      ))
  )

(define (exp-special-form? exp)
  (and (pair? exp) (lookup-special-form-compiler (car exp)))
  )
(define (compile-special-form exp)
  ((lookup-special-form-compiler (car exp)) (cdr exp))
  )

(define (compile-call exp)
  (let ((p (compile (car exp)))(args (map compile (cdr exp))))
    (lambda (env) 
      (let ((p-value (force-value (p env))))
        (let ((args-value (if (procedure? p-value) 
                            (map (lambda (arg) (force-value (arg env))) args) 
                            (map (lambda (arg) (make-thunk env arg)) args))))
          (call-native-or-script-procedure p-value args-value)))))
  )
;------------------------------
(define (compile-if exp)
  (let ((pred (compile (car exp)))(then (compile (cadr exp)))(alter (compile (caddr exp))))
    (lambda (env) (if (force-value (pred env)) (then env) (alter env))))
  )

(define (compile-cond exp)
  (define (cond->if exp)
    (let ((case-pred (caar exp))(case-body (cdar exp))(rest (cdr exp)))
      (if (null? rest)
        (cons 'begin case-body)
        (list 'if case-pred (cons 'begin case-body) (cond->if rest))))
    )
  (compile (cond->if exp))
  )

(define (compile-lambda exp)
  (let ((p-args (car exp))(body (compile-begin (cdr exp))))
    (lambda (env) (make-script-procedure env p-args body)))
  )

(define (compile-begin exp)
  (define (iter p procs)
    (if (null? procs)
      p
      (iter (lambda (env) (p env) ((car procs) env)) (cdr procs)))
    )
  (let ((procs (map compile exp)))
    (iter (car procs) (cdr procs)))
  )

(define (compile-define exp)
  (if (not (symbol? (car exp)))
    (compile-define (list (caar exp) (cons 'lambda (cons (cdar exp) (cdr exp)))))
    (let ((name (car exp))(value (compile (cadr exp))))
      (lambda (env) (env-define-variable! env name (value env)))))
  )

(define (compile-let exp)
  (define (to-call-lambda)
    (let ((args (map car (car exp)))(values (map cadr (car exp)))(body (cdr exp)))
      (cons (cons 'lambda (cons args body)) values))
    )
  (compile-call (to-call-lambda))
  )

(define (compile-set! exp)
  (let ((name (car exp))(depth false)(value (compile (cadr exp))))
    (lambda (env)
      (if depth
        (env-set-variable! env depth name (value env))
        (begin (set! depth (env-get-variable-depth env name))
               (env-set-variable! env depth name (value env))))))
  )

(define (compile-and exp)
  (define (iter p procs)
    (if (null? procs)
      p
      (let ((post (iter (car procs) (cdr procs))))
        (lambda (env) (and (p env) (post env)))))
    )
  (let ((procs (map compile exp)))
    (iter (car procs) (cdr procs)))
  )

(define (compile-or exp)
  (define (iter p procs)
    (if (null? procs)
      p
      (let ((post (iter (car procs) (cdr procs))))
        (lambda (env) (or (p env) (post env)))))
    )
  (let ((procs (map compile exp)))
    (iter (car procs) (cdr procs)))
  )

(define (compile-quote exp)
  (let ((e (car exp)))
    (lambda (env) e))
  )

(define (compile-eval! exp)
  (let ((value (compile (car exp))))
    (lambda (env) (eval env (value env))))
  )
;------------------------------
(define the-special-forms (make-hasheq))

(define (register-special-form-compiler symbol proc)
  (hash-set! the-special-forms symbol proc)
  )

(define (lookup-special-form-compiler symbol)
  (hash-ref the-special-forms symbol false)
  )

;------------------------------
(define G (make-env empty))

(define (register-builtin name var)
  (env-define-variable! G name var)
  )
;------------------------------
(define (compile exp)
  (cond
    ((exp-literal? exp) (compile-literal exp))
    ((exp-variable? exp) (compile-variable exp))
    ((exp-special-form? exp) (compile-special-form exp))
    (else (compile-call exp)))
  )

(define (execute env code)
  (code env)
  )

(define (eval env exp)
  (force-value (execute env (compile exp)))
  )

;------------------------------
(define (script-apply p . args)
  (define (flatten-variadic-args args)
    (cond
      ((null? args) empty)
      ((null? (cdr args)) (if (list? (car args)) (map force-value (car args)) (list (car args))))
      (else (cons (car args) (flatten-variadic-args (cdr args)))))
    )
  (call-native-or-script-procedure p (flatten-variadic-args args))
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
  (let ((builtins (list 
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
                          (cons 'if compile-if)
                          (cons 'cond compile-cond)
                          (cons 'define compile-define)
                          (cons 'lambda compile-lambda)
                          (cons 'and compile-and)
                          (cons 'or compile-or)
                          (cons 'let compile-let)
                          (cons 'quote compile-quote)
                          (cons 'begin compile-begin)
                          (cons 'set! compile-set!)
                          (cons 'eval! compile-eval!)
                          )))
    (for-each (lambda (p) (register-builtin (car p) (cdr p))) builtins)
    (for-each (lambda (p) (register-special-form-compiler (car p) (cdr p))) spectial-forms)
    (register-builtin-script-procedures)
    )
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

;------------------------------
; exercise 25

; applicative order
;(define (unless b usual exception)
;  (if b exception usual)
;  )
;(define (factoral n)
;  (unless (= n 1) (* n (factoral (- n 1))) 1)
;  )
;(factoral 10)

; normal order
;(eval G
;      '(begin
;         (define (unless b usual exception)
;           (if b exception usual)
;           )
;         (define (factoral n)
;           (unless (= n 1) (* n (factoral (- n 1))) 1)
;           )
;         (factoral 10)))
