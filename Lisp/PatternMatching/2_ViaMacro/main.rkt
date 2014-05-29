#lang racket

;------------------------------
(define-syntax match
  (syntax-rules (else guard)
    [(_ (e ...) rest-case ...)
     (let (v (e ...))
       (match v rest-case ...))]
    [(_ exp) (error "Can't match:" exp)]
    [(_ exp (else e ...)) (begin e ...)]
    [(_ exp (pat (guard g) e ...) rest-case ...)
     (let ([fk (lambda () (match exp rest-case ...))])
       (match-case exp pat (if g (begin e ...) (fk)) (fk)))]
    [(_ exp (pat e ...) rest-case ...)
     (let ([fk (lambda () (match exp rest-case ...))])
       (match-case exp pat (begin e ...) (fk)))
     ]
    )
  )

(define-syntax match-case
  (syntax-rules (quote)
    [(_ exp (quote e) sk fk)
     (if (equal? exp (quote e)) sk fk)]
    [(_ exp () sk fk) 
     (if (empty? exp) sk fk)] 
    [(_ exp (x . y) sk fk) 
     (if (pair? exp)
       (match-case 
         (car exp) x 
         (match-case (cdr exp) y sk fk)
         fk)
       fk)]
    [(_ exp a sk fk) 
     (let ([a exp]) sk)])
  )

;;------------------------------
(define (syntax-dispatch exp)
  (match exp
         [x (guard (number? x)) (printf "number    => ~a\n" x)]
         [x (guard (string? x)) (printf "string    => ~a\n" x)]
         [x (guard (symbol? x)) (printf "symbol    => ~a\n" x)]
         [('quote x) (printf "quote    => ~a\n" x)]
         [('if pred then else) (printf "if   => pred=~a, then=~a, else=~a\n" pred then else)]
         [('begin e . rest) (printf "begin   => e=~a, rest=~a\n" e rest)]
         [('lambda args . exps) (printf "lambda     => args=~a, exps=~a\n" args exps)]
         [('define (var . args) . exps) (symbol? var) (printf "define lambda    => var=~a,args=~a,exps=~a\n" var args exps)]
         [('define var value) (symbol? var) (printf "define variable    => var=~a,value=~a\n" var value)]
         [('set! var value) (symbol? var) (printf "set!     => var=~a,value=~a\n" var value)]
         [('cond (case-pred . case-exp) . rest-case) (printf "cond  => case-pred=~a,case-exp=~a,rest-case=~a\n" case-pred case-exp rest-case)]
         [('and e . rest) (printf "and     => e=~a, rest=~a\n" e rest)]
         [('or e . rest) (printf "or     => e=~a, rest=~a\n" e rest)]
         [('let ([name value] . rest-name-values) . exps) (printf "let  => name=~a,value=~a,rest=~a,exps=~a\n" name value rest-name-values exps)]
         [('let var ([name value] . rest-name-values) . exps) (printf "named let  => var=~a,name=~a,value=~a,rest=~a,exps=~a\n" var name value rest-name-values exps)]
         [('let* ([name value] . rest-name-values) . exps) (printf "let*  => name=~a,value=~a,rest=~a,exps=~a\n" name value rest-name-values exps)]
         [('letrec ([name value] . rest-name-values) . exps) (printf "letrec  => name=~a,value=~a,rest=~a,exps=~a\n" name value rest-name-values exps)]
         [(p . args) (printf "application   => p=~a,args=~a\n" p args)]
         )
  )
;------------------------------
(for-each syntax-dispatch
          '(1
            "1"
            a
            '(1 2 3)
            (if (= 0 n) a (fib b (+ a b) (- n 1)))
            (begin 1 2 (print x) x)
            (lambda (a b) (define c (+ a b)) (+ c c))
            (define a 1)
            (define (add a b) (+ a b))
            (set! a (+ a 1))
            (cond [(= n 0) a][(= n 1) b][else (+ a b)])
            (and (pair? p) (eq? (car p) 'quote))
            (or (empty? p) (eq? (car p) 'eof))
            (let ([x 1]) (define z (+ x y)) (+ x y))
            (let name ([x 1][y 2]) (define z (+ x y)) (+ x y))
            (let* ([x 1][y 2]) (define z (+ x y)) (+ x y))
            (letrec ([x 1][y 2]) (define z (+ x y)) (+ x y))
            (+ 2 1)
            ((lambda (a b) (+ a b)) 2 1)
            )
          )
