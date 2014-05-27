#lang racket

(provide (all-defined-out))
;------------------------------
; env
(define (make-env prev-env)
  (cons (make-hasheq) prev-env)
  )
(define (env-define! env name value)
  (hash-set! (car env) name value)
  )
(define (env-lookup-depth env name)
  (cond
    [(empty? env) (error "Can't find variable: " name)]
    [(hash-has-key? (car env) name) 0]
    [else (+ 1 (env-lookup-depth (cdr env) name))])
  )
(define (env-lookup env depth name)
  (if (zero? depth)
    (hash-ref (car env) name)
    (env-lookup (cdr env) (- depth 1) name))
  )
(define (env-set! env depth name value)
  (if (zero? depth)
    (hash-set! (car env) name value)
    (env-set! (cdr env) (- depth 1) name value))
  )
;------------------------------
; variable-location
(define (make-variable-location name)
  (mcons false name)
  )
(define (variable-location-read loc env)
  (if (mcar loc)
    (env-lookup env (mcar loc) (mcdr loc))
    (begin (set-mcar! loc (env-lookup-depth env (mcdr loc)))
           (env-lookup env (mcar loc) (mcdr loc))))
  )
(define (variable-location-write loc env value)
  (if (mcar loc)
    (env-set! env (mcar loc) (mcdr loc) value)
    (begin (set-mcar! loc (env-lookup-depth env (mcdr loc)))
           (env-set! env (mcar loc) (mcdr loc) value)))
  )
;------------------------------
(define (setup-call-env-for-script-procedure env p-args args)
  (cond
    [(empty? p-args) (if (empty? args) 'ok (error "argument count mismatch! " p-args args)) env]
    [(symbol? p-args) (env-define! env p-args args) env]
    [else (env-define! env (car p-args) (car args)) 
          (setup-call-env-for-script-procedure env (cdr p-args) (cdr args))])
  )
(define (make-script-procedure env p-args p-body)
  (cons 'tag-script-procedure 
        (lambda (args succ fail)
          (p-body (setup-call-env-for-script-procedure (make-env env) p-args args) succ fail)))
  )
(define (make-script-procedure-with-native-procedure p)
  (cons 'tag-script-procedure p)
  )
(define (script-procedure? value)
  (and (pair? value) (eq? (car value) 'tag-script-procedure))
  )
(define (call-native-or-script-procedure p args succ fail)
  (if (script-procedure? p)
    ((cdr p) args succ fail)
    (succ (apply p args) fail))
  )
;------------------------------
; exp
(define (exp-literal? exp)
  (or (number? exp) (string? exp))
  )
(define (exp-variable? exp)
  (symbol? exp)
  )
(define (compile-exp-literal exp)
  (lambda (env succ fail) (succ exp fail))
  )
(define (compile-exp-variable exp)
  (let ([loc (make-variable-location exp)])
    (lambda (env succ fail) (succ (variable-location-read loc env) fail)))
  )
(define (compile-exp-call exp)
  (define (combine-arg-cexps cexps)
    (if (empty? cexps)
      (lambda (env succ fail) (succ empty fail))
      (let ([rest-cexp (combine-arg-cexps (cdr cexps))])
        (lambda (env succ fail)
          ((car cexps) 
           env
           (lambda (v fail2)
             (rest-cexp 
               env
               (lambda (rest-args fail3)
                 (succ (cons v rest-args) fail3))
               fail2))
           fail))))
    )
  (let ([p-cexp (compile (car exp))]
        [arg-cexp (combine-arg-cexps (map compile (cdr exp)))])
    (lambda (env succ fail)
      (p-cexp env
              (lambda (p fail2)
                (arg-cexp env 
                          (lambda (args fail3)
                            (call-native-or-script-procedure p args succ fail3)) 
                          fail2))
              fail)))
  )
;------------------------------
; special form compiler
(define (compile-quote exp)
  (lambda (env succ fail) (succ (cadr exp) fail))
  )
(define (compile-if exp)
  (let ([pred-cexp (compile (cadr exp))]
        [then-cexp (compile (caddr exp))]
        [else-cexp (compile (cadddr exp))])
    (lambda (env succ fail)
      (pred-cexp env 
                 (lambda (b fail2)
                   (if b
                     (then-cexp env succ fail2)
                     (else-cexp env succ fail2))) 
                 fail)))
  )
(define (compile-begin exp)
  (define (combine-cexps cexp cexps)
    (if (empty? cexps)
      cexp
      (combine-cexps 
        (lambda (env succ fail)
          (cexp env 
                (lambda (v fail2)
                  ((car cexps) env succ fail2)) 
                fail)) 
        (cdr cexps)))
    )
  (let ([cexps (map compile (cdr exp))])
    (combine-cexps (car cexps) (cdr cexps)))
  )
(define (compile-lambda exp)
  (let ([exp-args (cadr exp)][cexp-body (compile-begin (cons 'begin (cddr exp)))])
    (lambda (env succ fail)
      (succ (make-script-procedure env exp-args cexp-body) fail)))
  )
(define (compile-define exp)
  (if (symbol? (cadr exp))
    (let ([name (cadr exp)][cexp (compile (caddr exp))])
      (lambda (env succ fail)
        (cexp env 
              (lambda (v fail2) (env-define! env name v) (succ empty fail2)) 
              fail)))
    (compile-define (list 'define (caadr exp) (append (list 'lambda (cdr (cadr exp))) (cddr exp)))))
  )
(define (compile-set! exp)
  (let ([loc (make-variable-location (cadr exp))][cexp (compile (caddr exp))])
    (lambda (env succ fail)
      (cexp env 
            (lambda (new-value fail2)
              (let ([old-value (variable-location-read loc env)])
                (variable-location-write loc env new-value)
                (succ empty (lambda ()
                              (variable-location-write loc env old-value)
                              (fail2))))
              )
            fail)))
  )
(define (compile-amb exp)
  (let ([cexps (map compile (cdr exp))])
    (lambda (env succ fail)
      ((let register-fail ([cexps cexps])
         (if (empty? cexps)
           fail
           (let ([rest-fail (register-fail (cdr cexps))])
             (lambda ()
               ((car cexps) env succ rest-fail))))))))
  )
(define (compile-eval! exp)
  (let ([exp (cadadr exp)])
    (lambda (env succ fail) (succ (eval env exp) fail)))
  )
;------------------------------
; derived form compiler
(define (compile-cond exp)
  (define (cond->if case-body-list)
    (let ((case-pred (caar case-body-list))
          (case-body (cdar case-body-list))
          (rest (cdr case-body-list)))
      (if (empty? rest) 
        (cons 'begin case-body)
        (list 'if case-pred (cons 'begin case-body) (cond->if rest))))
    )
  (compile-if (cond->if (cdr exp)))
  )
(define (compile-let exp)
  (define (let->call exp)
    (let ((names (map car (cadr exp)))
          (values (map cadr (cadr exp)))
          (body (cddr exp)))
      (cons (append (list 'lambda names) body) values))
    )
  (compile-exp-call (let->call exp))
  )
(define (compile-and exp)
  (define (and->if values)
    (if (empty? (cdr values))
      (car values)
      (list 'if (car values) (and->if (cdr values)) 'false))
    )
  (compile-if (and->if (cdr exp)))
  )
(define (compile-or exp)
  (define (or->let values)
    (if (empty? (cdr values))
      (car values)
      (list 'let (list (list 'value (car values))) (list 'if 'value 'value (or->let (cdr values)))))
    )
  (compile-let (or->let (cdr exp)))
  )
;------------------------------
; G env
(define G (make-env empty))
(define (builtin-register name value)
  (env-define! G name value)
  )
;------------------------------
; special forms
(define the-special-forms (make-hasheq))
(define (special-form-register name compiler)
  (hash-set! the-special-forms name compiler)
  )
(define (special-form-lookup name)
  (hash-ref the-special-forms name false)
  )
;------------------------------
; eval
(define (compile exp)
  (cond
    ((exp-literal? exp) (compile-exp-literal exp))
    ((exp-variable? exp) (compile-exp-variable exp))
    ((special-form-lookup (car exp)) => (lambda (p) (p exp)))
    (else (compile-exp-call exp))
    )
  )
(define (eval env exp)
  (let ((result empty))
    ((compile exp) env 
                   (lambda (value fail) (set! result (cons value result)) (fail)) 
                   (lambda () (reverse result))))
  )
;------------------------------
; setup
(define (script-apply args succ fail)
  (call-native-or-script-procedure 
    (car args)
    (let flatten-args ([args (cdr args)])
      (cond
        [(empty? args) empty]
        [(empty? (cdr args)) (if (list? (car args)) (car args) (list (car args)))]
        [else (cons (car args) (flatten-args (cdr args)))])) 
    succ fail)
  )
(define (script-call/cc args succ fail)
  (let ([f (car args)])
    (call-native-or-script-procedure 
      f 
      (list (make-script-procedure-with-native-procedure
              (lambda (args succ2 fail2)
                (succ (car args) fail2))))
      succ fail))
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
             (if (empty? l)
               init
               (foldl proc (proc (car l) init) (cdr l)))
             )
           (define (map proc l)
             (if (empty? l)
               empty
               (cons (proc (car l)) (map proc (cdr l))))
             )
           (define (filter pred l)
             (cond
               ((empty? l) empty)
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
  (map (lambda (pair) (special-form-register (car pair) (cdr pair))) 
       (list (cons 'quote compile-quote)
             (cons 'if compile-if)
             (cons 'begin compile-begin)
             (cons 'lambda compile-lambda)
             (cons 'define compile-define)
             (cons 'set! compile-set!)
             (cons 'amb compile-amb)
             (cons 'cond compile-cond)
             (cons 'let compile-let)
             (cons 'and compile-and)
             (cons 'or compile-or)
             (cons 'eval! compile-eval!)
             ))
  (map (lambda (pair) (builtin-register (car pair) (cdr pair))) 
       (list (cons '+ +)
             (cons '- -)
             (cons '* *)
             (cons '/ /)
             (cons '= =)
             (cons '< <)
             (cons '<= <=)
             (cons '> >)
             (cons '>= >=)
             (cons 'remainder remainder)
             (cons 'quotient quotient)
             (cons 'not not)
             (cons 'true true)
             (cons 'false false)
             (cons 'cons cons)
             (cons 'car car)
             (cons 'cdr cdr)
             (cons 'empty empty)
             (cons 'empty? empty?)
             (cons 'list list)
             (cons 'append append)
             (cons 'length length)
             (cons 'range range)
             (cons 'identity identity)
             (cons 'apply (make-script-procedure-with-native-procedure script-apply))
             (cons 'call/cc (make-script-procedure-with-native-procedure script-call/cc))
             (cons 'current-inexact-milliseconds current-inexact-milliseconds)
             (cons 'printf printf)
             (cons 'pretty-print pretty-print)
             (cons 'eq? eq?)
             ))
  (register-builtin-script-procedures)
  )
;------------------------------
; test and benchmark
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

  ; call/cc + amb
  (eval G 
        '(let ([main-k false])
           (let ([state (call/cc 
                          (lambda (k)
                            (set! main-k k)
                            (k (amb 1 2 3 4 5 6))))])
             (cond
               [(< state 5) (printf "~a\n" state) (amb)]
               [else state])))
        )
  )
(define (run-benchmark)
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
;(run-benchmark)
;------------------------------
(eval G
      '(begin
         (define (assert b)
           (if b 'ok (amb))
           )
         (define (an-integer-between a b)
           (assert (<= a b))
           (amb a (an-integer-between (+ a 1) b))
           )
         (define (one-of l)
           (assert (not (empty? l)))
           (amb (car l) (one-of (cdr l)))
           )
         ))

;------------------------------
; exercise 35

;(eval G
;      '(begin
;         (define (a-pythagorean-triple-between low high)
;           (let ([i (an-integer-between low high)])
;             (let ([j (an-integer-between i high)])
;               (let ([k (an-integer-between j high)])
;                 (assert (= (+ (* i i) (* j j)) (* k k)))
;                 (list i j k))))
;           )
;         (a-pythagorean-triple-between 1 100)
;         )
;      )
