#lang racket

(provide (all-defined-out))
;------------------------------
(define (make-env prev-env)
  (cons (make-hasheq) prev-env)
  )
(define (env-define env name value)
  (hash-set! (car env) name value)
  )
(define (env-set env name value)
  (cond 
    [(empty? env) (error "invalid set: " name value)]
    [(hash-has-key? (car env) name) (hash-set! (car env) name value)]
    [else (env-set (cdr env) name value)])
  )
(define (env-get env name)
  (cond
    [(empty? env) (error "invalid get: " name)]
    [(hash-has-key? (car env) name) (hash-ref (car env) name false)]
    [else (env-get (cdr env) name)])
  )
;------------------------------
(define (make-varaible-location name)
  name
  )
(define (variable-location-read loc env)
  (env-get env loc)
  )
(define (variable-location-write loc env value)
  (env-set env loc value)
  )
;------------------------------
(define tag-script-procedure (list 'script-procedure))
(define (setup-script-procedure-call-env env p-args args)
  (cond
    [(empty? p-args) (if (empty? args) env (error "Argument count mismatch!"))]
    [(symbol? p-args) (env-define env p-args args) env]
    [else (env-define env (car p-args) (car args)) 
          (setup-script-procedure-call-env env (cdr p-args) (cdr args))])
  )
(define (make-script-procedure env p-args p-exps)
  (cons tag-script-procedure
        (lambda (args k)
          (p-exps (setup-script-procedure-call-env (make-env env) p-args args) k)))
  )
(define (make-script-procedure-with-native-procedure p)
  (cons tag-script-procedure p)
  )
(define (script-procedure? p)
  (and (pair? p) (eq? (car p) tag-script-procedure))
  )
(define (call-native-or-script-procedure p args k)
  (if (script-procedure? p)
    ((cdr p) args k)
    (k (apply p args)))
  )
;------------------------------
(define (compile exp)
  (cond 
    [(or (string? exp) (number? exp)) 
     (lambda (env k) (k exp))]
    [(symbol? exp) 
     (let ([loc (make-varaible-location exp)])
       (lambda (env k) (k (variable-location-read loc env))))]
    [else 
      (match 
        exp
        [(list 'quote e)
         (lambda (env k) (k e))]
        [(list 'if pred then else)
         (let ([pred (compile pred)][then (compile then)][else (compile else)])
           (lambda (env k)
             (pred env (lambda (b)
                         (if b
                           (then env k)
                           (else env k))))))
         ]
        [(list 'begin exp-list ...)
         (let ([exp-list (map compile exp-list)])
           (let combine ([first (car exp-list)][rest (cdr exp-list)])
             (if (empty? rest) 
               first
               (combine
                 (lambda (env k)
                   (first env (lambda (v)
                                ((car rest) env k))))
                 (cdr rest)))))
         ]
        [(list 'lambda args exp-list ...)
         (let ([exp-list (compile (cons 'begin exp-list))])
           (lambda (env k)
             (k (make-script-procedure env args exp-list))))
         ]
        [(list 'define name value-list ...)
         (if (symbol? name)
           (let ([value (compile (car value-list))])
             (lambda (env k)
               (value env (lambda (v)
                            (env-define env name v)
                            (k 'define-ok)))))
           (compile `(define ,(car name) (lambda ,(cdr name) ,@value-list))))
         ]
        [(list 'set! name value)
         (let ([loc (make-varaible-location name)][value (compile value)])
           (lambda (env k)
             (value env (lambda (v)
                          (variable-location-write loc env v)
                          (k 'set-ok)))))
         ]
        [(list 'let (list (list name-list value-list) ...) exp-list ...)
         (compile `((lambda ,name-list ,@exp-list) ,@value-list))
         ]
        [(list 'let* (cons (list name value) rest-list) exp-list ...)
         (if (empty? rest-list)
           (compile `(let ([,name ,value]) ,@exp-list))
           (compile `(let ([,name, value]) (let* ,rest-list ,@exp-list))))
         ]
        [(list 'letrec (list (list name-list value-list) ...) exp-list ...)
         (compile `((lambda ,name-list 
                      ,@(map (lambda (name value) `(set! ,name ,value)) name-list value-list) 
                      ,@exp-list) 
                    ,@(map (lambda (e) ''undefined) name-list)))
         ]
        [(list 'let name (list (list name-list value-list) ...) exp-list ...)
         (compile `((lambda () ((define ,name (lambda ,name-list ,@exp-list)) (,name ,@value-list)))))
         ]
        [(cons 'cond (cons (list pred exp-list ...) rest-list))
         (if (empty? rest-list)
           (compile `(begin ,pred ,@exp-list))
           (compile `(if ,pred (begin ,@exp-list) (cond ,@rest-list))))
         ]
        [(cons 'and (cons e rest-list))
         (if (empty? rest-list)
           (compile e)
           (compile `(if ,e (and ,@rest-list) false)))
         ]
        [(cons 'or (cons e rest-list))
         (if (empty? rest-list)
           (compile e)
           (let ([tmp-name (gensym)])
             (compile `(let ([,tmp-name ,e]) (if ,tmp-name ,tmp-name (or ,@rest-list))))))
         ]
        [(list 'eval (list 'quote e))
         (lambda (env k)
           (k (eval env e)))
         ]
        [(list p arg-list ...)
         (let ([p (compile p)]
               [arg-list 
                 (let combine ([arg-list (map compile arg-list)])
                   (if (empty? arg-list)
                     (lambda (env k) (k empty))
                     (let ([rest (combine (cdr arg-list))])
                       (lambda (env k)
                         ((car arg-list) env (lambda (v)
                                               (rest env (lambda (values)
                                                           (k (cons v values))))))))))])
           (lambda (env k)
             (p env (lambda (p)
                      (arg-list env (lambda (args)
                                      (call-native-or-script-procedure p args k)))))))
         ]
        )])
  )
(define (eval env exp)
  ((compile exp) env identity)
  )
;------------------------------
(define G (make-env empty))
(define (builtin-register name value)
  (env-define G name value)
  )
;------------------------------
(define (script-apply args k)
  (define (flatten-variadic-args args)
    (cond
      [(empty? args) empty]
      [(empty? (cdr args)) (if (list? (car args)) (car args) (list (car args)))]
      [else (cons (car args) (flatten-variadic-args (cdr args)))])
    )
  (call-native-or-script-procedure (car args) (flatten-variadic-args (cdr args)) k)
  )
(define (script-call/cc args k)
  (let ([p (car args)])
    (call-native-or-script-procedure p (list k) k))
  )

(builtin-register 'apply (make-script-procedure-with-native-procedure script-apply))
(builtin-register 'call/cc (make-script-procedure-with-native-procedure script-call/cc))
