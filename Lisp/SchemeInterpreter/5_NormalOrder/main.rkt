#lang racket

(require compatibility/mlist)
;------------------------------
(define (make-env pre-env formal-list actual-list)
  (mcons (mappend 
           (let bind-arguments ([formal-list formal-list][actual-list actual-list][result empty])
             (cond
               [(empty? formal-list) (if (empty? actual-list) result (error "Argument count mismatch:" actual-list))]
               [(symbol? formal-list) (mcons (mcons formal-list actual-list) result)]
               [else (bind-arguments (cdr formal-list) (cdr actual-list) (mcons (mcons (car formal-list) (car actual-list)) result))]))
           (if (empty? pre-env) empty (mcar pre-env)))
         false)
  )
(define (env-define env name value)
  (set-mcar! env (mcons (mcons name value) (mcar env)))
  )
(define (env-set env name value)
  (set-mcdr! (massq name (mcar env)) value)
  )
(define (env-get env name)
  (mcdr (massq name (mcar env)))
  )
;------------------------------
(define the-argument-passing-mechanism 'call-by-value)
(define the-thunk-tag '(thunk))
(define (thunk? v)
  (and (pair? v) (eq? (car v) the-thunk-tag))
  )
(define (make-thunk env exp)
  (cond
    [(eq? the-argument-passing-mechanism 'call-by-value)
     (eval env exp)]
    [(eq? the-argument-passing-mechanism 'call-by-name)
     (cons the-thunk-tag (lambda () (force (eval env exp))))]
    [(eq? the-argument-passing-mechanism 'call-by-need)
     (let ([value false])
       (cons the-thunk-tag 
             (lambda ()
               (if env
                 (begin (set! value (force (eval env exp)))
                        (set! env false)
                        value)
                 value))))]
    [else (error "Unknown argument passing mechanism:" the-argument-passing-mechanism)]))
(define (force v)
  (if (thunk? v) ((cdr v)) v)
  )
;------------------------------
(define the-script-procedure-tag '(script-procedure))
(define (make-script-procedure p)
  (cons the-script-procedure-tag p)
  )
(define (script-procedure? v)
  (and (pair? v) (eq? (car v) the-script-procedure-tag))
  )
(define (apply-application env p actual-list)
  (let ([p (force (eval env p))])
    (if (script-procedure? p)
      (apply (cdr p) (map (lambda (e) (make-thunk env e)) actual-list))
      (apply p (map (lambda (e) (force (eval env e))) actual-list))))
  )
;------------------------------
(define (eval env exp)
  (match 
    exp
    [(? (lambda (x) (or (string? x) (number? x))) x) 
     x]
    [(? symbol? x) 
     (env-get env x)]
    [`(quote ,x)
      x]
    [`(if ,pred ,then ,else) 
      (if (force (eval env pred)) (eval env then) (eval env else))]
    [`(begin ,exp-list ...) 
      (foldl (lambda (e _) (eval env e)) empty exp-list)]
    [`(lambda ,formal-list ,exp-list ...) 
      (make-script-procedure
        (lambda args (eval (make-env env formal-list args) (cons 'begin exp-list))))]
    [`(define (,(? symbol? name) ,formal-list ...) ,exp-list ...) 
      (eval env `(define ,name (lambda ,formal-list ,@exp-list)))]
    [`(define ,(? symbol? name) ,value) 
      (env-define env name (eval env value))]
    [`(set! ,(? symbol? name) ,value) 
      (env-set env name (eval env value))]
    [`(cond (,case-pred ,case-exp-list ...) ,case-list ...) 
      (if (empty? case-list)
        (eval env `(begin ,case-pred ,@case-exp-list))
        (eval env `(if ,case-pred (begin ,@case-exp-list) (cond ,@case-list))))]
    [`(and ,exp0 ,exp-list ...) 
      (if (empty? exp-list)
        (eval env exp0)
        (eval env `(if ,exp0 (and ,@exp-list) false)))]
    [`(or ,exp0 ,exp-list ...) 
      (if (empty? exp-list)
        (eval env exp0)
        (let ([tmpname (gensym)])
          (eval env `(let ([,tmpname ,exp0]) (if ,tmpname ,tmpname (or ,@exp-list))))))]
    [`(let ,(? symbol? fname) ,name-value-list ,exp-list ...) 
      (eval env `((lambda () 
                    (define ,fname (lambda ,(map car name-value-list) ,@exp-list)) 
                    (,fname ,@(map cadr name-value-list)))))]
    [`(let ,name-value-list ,exp-list ...) 
      (eval env `((lambda ,(map car name-value-list) ,@exp-list) 
                  ,@(map cadr name-value-list)))]
    [`(let* ((,(? symbol? name) ,value) ,name-value-list ...) ,exp-list ...) 
      (if (empty? name-value-list)
        (eval env `((lambda (,name) ,@exp-list) ,value))
        (eval env `((lambda (,name) (let* ,name-value-list ,@exp-list)) ,value)))]
    [`(letrec ,name-value-list ,exp-list ...) 
      (eval env `((lambda ,(map car name-value-list) 
                    ,@(map (lambda (nv) `(set! ,(car nv) ,(cadr nv))) name-value-list) ,@exp-list) 
                  ,@(map (lambda (_) ''undefined) name-value-list)))]
    [`(,p ,actual-list ...) 
      (apply-application env p actual-list)]
    )
  )
;------------------------------
(define G ((lambda (pairs)
             (make-env empty (map car pairs) (map cadr pairs)))
           `(
             (true ,true) (false ,false) (else ,true)
             (+ ,+) (- ,-) (* ,*) (/ ,/) (quotient ,quotient) (remainder ,remainder) (expt ,expt) (sqr ,sqr) (sqrt ,sqrt)
             (= ,=) (< ,<) (<= ,<=) (> ,>) (>= ,>=) (not ,not)
             (set-argument-passing-mechanism ,(lambda (v) (set! the-argument-passing-mechanism v)))
             (empty? ,empty?) (empty ,empty)
             (raw-pair? ,pair?) (raw-cons ,cons) (raw-car ,car) (raw-cdr ,cdr)
             (eq? ,eq?) (even? ,even?) (odd? ,odd?)
             (raw-print ,print) (newline ,newline)
             (identity ,identity) (void ,void)
             )))
;------------------------------
(do ([datum (read) (read)])
  ((eof-object? datum))
  (let ([v (force (eval G datum))])
    (if (void? v)
      'ok
      (pretty-print v))))
