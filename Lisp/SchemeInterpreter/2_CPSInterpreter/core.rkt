#lang racket

(provide (all-defined-out))
;------------------------------
(define the-void (void))
;------------------------------
(define-syntax match 
  (syntax-rules (guard else)
    [(_ (ve ...) rest-case ...)
     (let ([v (ve ...)])
       (match v rest-case ...))]
    [(_ v) 
     (error "Can't match " v)]
    [(_ v (else e ...))
     (begin e ...)]
    [(_ v (pat (guard b) e ...) rest-case ...) 
     (let ([fk (lambda () (match v rest-case ...))])
       (match-pattern v pat (if b (begin e ...) (fk)) (fk)))]
    [(_ v (pat e ...) rest-case ...)
     (let ([fk (lambda () (match v rest-case ...))])
       (match-pattern v pat (begin e ...) (fk)))]
    )
  )
(define-syntax match-pattern
  (syntax-rules (quote)
    [(_ v () sk fk) 
     (if (empty? v) sk fk)]
    [(_ v (quote e) sk fk)
     (if (equal? v (quote e)) sk fk)]
    [(_ v (x . y) sk fk)
     (if (pair? v)
       (match-pattern (car v) x 
                      (match-pattern (cdr v) y sk fk)
                      fk)
       fk)]
    [(_ v var sk fk)
     (let ([var v]) sk)]
    )
  )
;------------------------------
(define (find-definitions exps arg-names)
  (define (flatten-variadic-arg-names names)
    (cond
      [(empty? names) empty]
      [(symbol? names) (list names)]
      [else (cons (car names) (flatten-variadic-arg-names (cdr names)))])
    )
  (reverse
    (let find ([exps exps][names (flatten-variadic-arg-names arg-names)])
      (cond
        [(empty? exps) names]
        [(not (pair? (car exps))) (find (cdr exps) names)]
        [(eq? 'define (caar exps)) 
         (let ([name (if (symbol? (cadar exps)) (cadar exps) (caadar exps))])
           (find (cdr exps) (cons name names)))]
        [(eq? 'begin (caar exps)) (find (cdr exps) (find (cdar exps) names))]
        [else (find (cdr exps) names)])))
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
(define (setup-script-procedure-call-env env p-args args)
  (cond
    [(empty? p-args) (if (empty? args) env (error "Argument count mismatch!"))]
    [(symbol? p-args) (env-define env p-args args) env]
    [else (env-define env (car p-args) (car args)) 
          (setup-script-procedure-call-env env (cdr p-args) (cdr args))])
  )
(define (make-script-procedure env p-args p-exps name-vec)
  (lambda (args k)
    (p-exps (setup-script-procedure-call-env (make-static-env env name-vec) p-args args) k))
  )
;------------------------------
(define the-macros (make-hasheq))
(define (macro? exp)
  (and (pair? exp) (hash-has-key? the-macros (car exp)))
  )
(define (macro-register name transformer)
  (if (hash-has-key? the-macros name)
    (hash-set! the-macros name (cons transformer (hash-ref the-macros name)))
    (hash-set! the-macros name (list transformer)))
  )
(define (macro-transform exp)
  (let try-transform ([transformers (hash-ref the-macros (car exp))])
    (cond
      [(empty? transformers) (error "Can't transform macro: " exp)]
      [((car transformers) exp) => identity]
      [else (try-transform (cdr transformers))]))
  )
(define (macro-match exp pattern sk fk)
  (cond
    [(eq? '_ pattern) (sk empty)]
    [(symbol? pattern) (sk (list (cons pattern exp)))]
    [(empty? pattern) (if (empty? exp) (sk empty) (fk exp pattern))]
    [(pair? pattern) 
     (if (pair? exp) 
       (macro-match (car exp) (car pattern)
                            (lambda (head-pairs) 
                              (macro-match (cdr exp) (cdr pattern) 
                                                   (lambda (tail-pairs) 
                                                     (sk (append head-pairs tail-pairs))) 
                                                   fk))
                            fk) 
       (fk pattern exp))]
    [else (error "Invalid pattern:" pattern)])
  )
(define (macro-pattern-names pattern)
  (cond
    [(eq? '_ pattern) empty]
    [(symbol? pattern) (list pattern)]
    [(empty? pattern) empty]
    [(pair? pattern) (append (macro-pattern-names (car pattern)) (macro-pattern-names (cdr pattern)))]
    [else (error "Invalid pattern:" pattern)])
  )
(define (make-transformer env pattern exp-list)
  (let* ([compiled-exp-list (compile (cons 'begin exp-list))]
         [p-args (macro-pattern-names pattern)]
         [p (make-script-procedure env p-args compiled-exp-list (list->vector (find-definitions exp-list p-args)))])
    (lambda (exp)
      (macro-match exp pattern 
                           (lambda (pairs)
                             (p (map cdr pairs) identity)) 
                           (lambda (exp pattern) false))))
  )
;------------------------------
(define (eval-quasiquote-single-ret env exp quote-depth)
  (let ([result (eval-quasiquote-multiple-ret env exp quote-depth empty)])
    (if (= 1 (length result))
      (car result)
      (error "Should return single:\n" exp result)))
  )
(define (eval-quasiquote-multiple-ret env exp quote-depth init-list)
  (cond 
    [(pair? exp)
     (cond
       [(eq? 'quote (car exp)) (cons exp init-list)]
       [(eq? 'quasiquote (car exp)) (cons (list (car exp) (eval-quasiquote-single-ret env (cadr exp) (+ quote-depth 1))) init-list)]
       [(eq? 'unquote (car exp)) 
        (if (= 1 quote-depth)
          (cons (eval env (cadr exp)) init-list)
          (cons (list (car exp) (eval-quasiquote-single-ret env (cadr exp) (- quote-depth 1))) init-list))]
       [(eq? 'unquote-splicing (car exp)) 
        (if (= 1 quote-depth)
          (foldl cons init-list (eval env (cadr exp)))
          (cons (list (car exp) (eval-quasiquote-single-ret env (cadr exp) (- quote-depth 1))) init-list))]
       [else 
         (cons (reverse (foldl (lambda (e init) (eval-quasiquote-multiple-ret env e quote-depth init)) empty exp)) init-list)])]
    [else (cons exp init-list)])
  )
;------------------------------
(define (compile exp)
  (match 
    exp
    [('defmacro name pattern . exp-list)
     (macro-register name (make-transformer G pattern exp-list))
     (lambda (env k) (k the-void))]
    [x (guard (macro? x)) (compile (macro-transform x))]
    [x (guard (string? x)) (lambda (env k) (k x))]
    [x (guard (number? x)) (lambda (env k) (k x))]
    [x (guard (symbol? x)) 
     (let ([loc (make-varaible-location x)])
       (lambda (env k) (k (variable-location-read loc env))))]
    [('quote e)
     (lambda (env k) (k e))]
    [('quasiquote e)
     (lambda (env k) (k (eval-quasiquote-single-ret env e 1)))]
    [('if pred then else)
     (let ([pred (compile pred)][then (compile then)][else (compile else)])
       (lambda (env k)
         (pred env (lambda (b)
                     (if b
                       (then env k)
                       (else env k))))))
     ]
    [('begin . exp-list)
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
    [('lambda args . exp-list)
     (let ([exp-list (compile (cons 'begin exp-list))][name-vec (list->vector (find-definitions exp-list args))])
       (lambda (env k)
         (k (make-script-procedure env args exp-list name-vec))))
     ]
    [('define name . value-list)
     (if (symbol? name)
       (let ([value (compile (car value-list))])
         (lambda (env k)
           (value env (lambda (v)
                        (env-define env name v)
                        (k the-void)))))
       (compile `(define ,(car name) (lambda ,(cdr name) ,@value-list))))
     ]
    [('set! name value)
     (let ([loc (make-varaible-location name)][value (compile value)])
       (lambda (env k)
         (value env (lambda (v)
                      (variable-location-write loc env v)
                      (k the-void)))))
     ]
    [('eval ('quote e))
     (lambda (env k)
       (k (eval env e)))
     ]
    [(p . arg-list)
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
                                  (p args k)))))))
     ]
    )
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
  ((car args) (flatten-variadic-args (cdr args)) k)
  )
(define (script-call/cc args k)
  ((car args)
   (list (lambda (args k2) (if (empty? args) (k the-void) (k (car args))))) 
   k)
  )
(define (native-procedure->script-procedure p)
  (lambda (args k) (k (apply p args)))
  )

(builtin-register 'apply script-apply)
(builtin-register 'call/cc script-call/cc)
(builtin-register 'the-void the-void)
(builtin-register 'expand-1 (lambda (args k) (k (macro-transform (car args)))))
