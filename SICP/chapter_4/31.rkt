#lang racket

(provide (all-defined-out))
;------------------------------
(define thunk-tag (list 'thunk))
(define (make-memoize-thunk env code)
  (cons thunk-tag 
        (lambda ()
          (if code
            (begin (set! env (force-value (code env)))
                   (set! code false)
                   env)
            env)))
  )
(define (make-thunk env code)
  (cons thunk-tag
        (lambda () 
          (force-value (code env))))
  )
(define (thunk? value)
  (and (pair? value) (eq? thunk-tag (car value)))
  )
(define (thunk-value value)
  ((cdr value))
  )
(define (force-value value)
  (if (thunk? value)
    (thunk-value value)
    value)
  )
;------------------------------
(define ref-tag (list 'ref))
(define (make-ref env name)
  (if (symbol? name)
    (list ref-tag env (make-variable-location name))
    (error "ref should be symbol:" name))
  )
(define (ref? ref)
  (and (pair? ref) (eq? (car ref) ref-tag))
  )
(define (read-ref ref)
  (read-variable-location (cadr ref) (caddr ref))
  )
(define (write-ref ref value)
  (write-variable-location (cadr ref) (caddr ref) value)
  )
;------------------------------
(define (make-variable-location name)
  (let ((depth false)(ref 0))
    (lambda (m env new-value)
      (if depth 'ok (set! depth (env-get-variable-depth env name)))
      (if (eq? ref 0) (set! ref (ref? (env-lookup-variable env depth name))) 'ok)
      (cond 
        ((eq? m 'get) 
         (let ((value (env-lookup-variable env depth name)))
           (if ref (read-ref value) value)))
        ((eq? m 'set) 
         (if ref
           (write-ref (env-lookup-variable env depth name) new-value)
           (env-set-variable! env depth name new-value)))
        (else (error "invalid message: " name m)))))
  )
(define (read-variable-location env loc)
  (loc 'get env true)
  )
(define (write-variable-location env loc value)
  (loc 'set env value)
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
    (else (env-define-variable! env (if (symbol? (car p-args)) (car p-args) (caar p-args)) (car args)) 
          (setup-call-env env (cdr p-args) (cdr args)))
    )
  )
(define (setup-call-env-by-call-type env p-args args caller-env)
  (define (define-arg-by-call-type p-arg arg)
    (cond
      ((symbol? p-arg) (env-define-variable! env p-arg (force-value (arg caller-env))))
      ((eq? (cadr p-arg) 'by-name) (env-define-variable! env (car p-arg) (make-thunk caller-env arg)))
      ((eq? (cadr p-arg) 'by-need) (env-define-variable! env (car p-arg) (make-memoize-thunk caller-env arg)))
      ((eq? (cadr p-arg) 'by-ref) (env-define-variable! env (car p-arg) (make-ref caller-env (arg caller-env))))
      (else (error "Invalid argument declaration: " p-arg)))
    )
  (cond
    ((symbol? p-args) (env-define-variable! env p-args (map (lambda (arg) (force-value (arg caller-env))) args)) env)
    ((null? p-args) (if (null? args) env (error "argument count mismatch!")))
    (else (define-arg-by-call-type (car p-args) (car args)) 
          (setup-call-env-by-call-type env (cdr p-args) (cdr args) caller-env))
    )
  )

(define (call-native-or-script-procedure p args)
  (if (procedure? p)
    (apply p args)
    (let ((p-env (car p))(p-args (cadr p))(p-body (caddr p)))
      (p-body (setup-call-env (make-env p-env) p-args args))))
  )

(define (call-native-or-script-procedure-with-codes env code-p code-args)
  (let ((p (force-value (code-p env))))
    (if (procedure? p)
      (apply p (map (lambda (arg) (force-value (arg env))) code-args))
      (let ((p-env (car p))(p-args (cadr p))(p-body (caddr p)))
        (p-body (setup-call-env-by-call-type (make-env p-env) p-args code-args env)))))
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
  (let ((loc (make-variable-location exp)))
    (lambda (env) (read-variable-location env loc)))
  )

(define (exp-special-form? exp)
  (and (pair? exp) (lookup-special-form-compiler (car exp)))
  )
(define (compile-special-form exp)
  ((lookup-special-form-compiler (car exp)) (cdr exp))
  )

(define (compile-call exp)
  (let ((p (compile (car exp)))(args (map compile (cdr exp))))
    (lambda (env) (call-native-or-script-procedure-with-codes env p args)))
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
  (let ((loc (make-variable-location (car exp)))(value (compile (cadr exp))))
    (lambda (env) (write-variable-location env loc (value env))))
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
                    (cons 'pair? pair?)
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
                    (cons 'eq? eq?)
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
(run-test)
(benchmark)

;------------------------------
; stream

;(eval G '(define (stream-cons a b) (lambda (first) (if first a b))))
;(eval G '(define (stream-cons a (b by-name)) (lambda (first) (if first a b))))
;(eval G '(define (stream-cons a (b by-need)) (lambda (first) (if first a b))))
(eval G '(define (stream-cons (a by-need) (b by-need)) (lambda (first) (if first a b))))

(eval G
      '(begin
         (define (stream-car pair) (pair true))
         (define (stream-cdr pair) (pair false))
         (define (stream-map proc . streams)
           (if (null? (car streams))
             empty
             (stream-cons (apply proc (map stream-car streams)) 
                          (apply stream-map proc (map stream-cdr streams))))
           )
         (define (stream->list s)
           (if (null? s)
             empty
             (cons (stream-car s) (stream->list (stream-cdr s))))
           )
         (define (stream-take s n)
           (if (= n 0)
             empty
             (stream-cons (stream-car s) (stream-take (stream-cdr s) (- n 1))))
           )
         (define (stream-ref s n)
           (if (= n 1)
             (stream-car s)
             (stream-ref (stream-cdr s) (- n 1)))
           )
         ))


(eval G
      '(begin
         (define ones (stream-cons 1 ones))
         (define integers (stream-cons 1 (stream-map + integers ones)))
         (define fib (stream-cons 0 (stream-cons 1 (stream-map + fib (stream-cdr fib)))))
         (pretty-print (stream->list (stream-take ones 10)))
         (pretty-print (stream->list (stream-take integers 10)))
         (pretty-print (stream->list (stream-take fib 10)))
         (timing (curry 2 stream-ref fib 20) 10)
         (stream-ref (stream-map (lambda (i) (printf "{~a}\n" i) i) integers) 10)
         ))

;------------------------------
(eval G
      '(begin
         (define (swap (a by-ref) (b by-ref)) 
           (let ((t a))
             (set! a b)
             (set! b t))
           )
         (define (test-swap)
           (let ((a 1)(b 2))
             (swap 'a 'b)
             (printf "after swap: ~a ~a\n" a b))
           )
         (define (test-ref-1)
           (let ((a 1)(b 2))
             ((lambda ((c by-ref) (d by-ref))
                ((lambda ((e by-ref)(g by-ref))
                   (let ((t e))
                     (set! t (+ t 10))
                     (pretty-print t)
                     (swap 'e 'g)))
                 'c 'd)
                ) 'a 'b)
             (printf "after-test-ref: ~a ~a\n" a b))
           )
         (test-swap)
         (test-ref-1)
         ))
