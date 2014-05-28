#lang racket

(provide (all-defined-out))
;------------------------------
(define the-void (void))
;------------------------------
(define (find-definitions exps arg-names)
  (define (flatten-variadic-arg-names names)
    (cond
      [(empty? names) empty]
      [(symbol? names) (list names)]
      [else (cons (car names) (flatten-variadic-arg-names (cdr names)))])
    )
  (list->vector
    (reverse
      (let find ([exps exps][names (flatten-variadic-arg-names arg-names)])
        (cond
          [(empty? exps) names]
          [(not (pair? (car exps))) (find (cdr exps) names)]
          [(eq? 'define (caar exps)) 
           (let ([name (if (symbol? (cadar exps)) (cadar exps) (caadar exps))])
             (find (cdr exps) (cons name names)))]
          [(eq? 'begin (caar exps)) (find (cdr exps) (find (cdar exps) names))]
          [else (find (cdr exps) names)]))))
  )
;------------------------------
(define (make-dynamic-env pre-env max-name-count)
  (list (make-vector max-name-count false) (make-vector max-name-count) pre-env)
  )
(define (make-static-env pre-env name-vec)
  (list name-vec (make-vector (vector-length name-vec)) pre-env)
  )
(define (env-define env name value)
  (let ([index (vector-memq name (car env))])
    (if index
      (vector-set! (cadr env) index value)
      (begin (set! index (vector-memq false (car env)))
             (vector-set! (car env) index name)
             (vector-set! (cadr env) index value))))
  )
(define (env-find-depth-index env name depth)
  (let ([index (vector-memq name (car env))])
    (if index
      (cons depth index)
      (env-find-depth-index (caddr env) name (add1 depth))))
  )
(define (env-get env depth index)
  (do ([depth depth (sub1 depth)][env env (caddr env)])
    ((zero? depth) (vector-ref (cadr env) index)))
  )
(define (env-set env depth index value)
  (do ([depth depth (sub1 depth)][env env (caddr env)])
    ((zero? depth) (vector-set! (cadr env) index value)))
  )
;------------------------------
(define (make-varaible-location name)
  (mcons false name)
  )
(define (variable-location-read loc env)
  (if (mcar loc)
    (env-get env (mcar loc) (mcdr loc))
    (begin (let ([depth-index (env-find-depth-index env (mcdr loc) 0)])
             (set-mcar! loc (car depth-index))
             (set-mcdr! loc (cdr depth-index)))
           (env-get env (mcar loc) (mcdr loc))))
  )
(define (variable-location-write loc env value)
  (if (mcar loc)
    (env-set env (mcar loc) (mcdr loc) value)
    (begin (let ([depth-index (env-find-depth-index env (mcdr loc) 0)])
             (set-mcar! loc (car depth-index))
             (set-mcdr! loc (cdr depth-index)))
           (env-set env (mcar loc) (mcdr loc) value)))
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
(define (make-script-procedure env p-args p-exps name-vec)
  (cons tag-script-procedure
        (lambda (args k)
          (p-exps (setup-script-procedure-call-env (make-static-env env name-vec) p-args args) k)))
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
         (let ([exp-list (compile (cons 'begin exp-list))][name-vec (find-definitions exp-list args)])
           (lambda (env k)
             (k (make-script-procedure env args exp-list name-vec))))
         ]
        [(list 'define name value-list ...)
         (if (symbol? name)
           (let ([value (compile (car value-list))])
             (lambda (env k)
               (value env (lambda (v)
                            (env-define env name v)
                            (k the-void)))))
           (compile `(define ,(car name) (lambda ,(cdr name) ,@value-list))))
         ]
        [(list 'set! name value)
         (let ([loc (make-varaible-location name)][value (compile value)])
           (lambda (env k)
             (value env (lambda (v)
                          (variable-location-write loc env v)
                          (k the-void)))))
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
         (compile `((lambda () (define ,name (lambda ,name-list ,@exp-list)) (,name ,@value-list))))
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
        [(list 'do (list (list name-list init-list update-list) ...) (list pred exit-list ...) do-list ...)
         (let ([pname (gensym)])
           (compile `(let ,pname ,(map (lambda (name init) (list name init)) name-list init-list) 
                       (if ,pred 
                         (begin the-void ,@exit-list)
                         (begin ,@do-list 
                                (,pname ,@update-list))))))
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
(define G (make-dynamic-env empty 256))
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
    (call-native-or-script-procedure 
      p 
      (list (make-script-procedure-with-native-procedure 
              (lambda (args k2) (if (empty? args) (k the-void) (k (car args)))))) 
      k))
  )

(builtin-register 'apply (make-script-procedure-with-native-procedure script-apply))
(builtin-register 'call/cc (make-script-procedure-with-native-procedure script-call/cc))
(builtin-register 'the-void the-void)
