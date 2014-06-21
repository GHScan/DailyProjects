#lang racket

(provide (all-defined-out))
;------------------------------
(define pure-builtins '(
                        + - * / % quotient remainder expt
                        = < <= > >= not
                        add1 sub1 zero? identity sqr sqrt
                        cons car cdr eq? equal? eqv? empty? null?
                        drop drop-right length append reverse last
                        current-inexact-milliseconds random eval
                        ))
(define side-effect-builtins '(
                               printf pretty-print print display exit
                               define set!
                               ))
(define (cps-of-builtin v)
  (if (or (memq v pure-builtins) (memq v side-effect-builtins))
    (string->symbol (string-append (symbol->string v) "&")) 
    v)
  )
;------------------------------
(define (atom? a)
  (not (or (pair? a) (empty? a)))
  )
(define (not-pair? a)
  (not (pair? a))
  )
;------------------------------
(define (cps e)
  (define (transform-list exp-list k)
    (let iter ([exp-list exp-list][result empty])
      (if (empty? exp-list)
        (k (reverse result))
        (transform (car exp-list) 
                   (lambda (v) (iter (cdr exp-list) (cons v result))))))
    )
  (define (fk->expk fk)
    (let* ([t (gensym)][expk `(lambda (,t) ,(fk t))])
      (match expk
             [`(lambda (,x) (,y ,x)) y]
             [else expk]))
    )
  (define (let-fk fk k)
    (match (fk->expk fk)
           [(? atom? x) (k (lambda (v) `(,x ,v)))]
           [`(lambda (,x) ,x) (k identity)]
           [x (let ([t (gensym)])
                `(let ([,t ,x]) ,(k (lambda (v) `(,t ,v)))))])
    )
  (define (transform e k)
    (match
      e
      [(? atom? x) (k e)]
      [`(quote ,x) (k e)]
      [`(lambda ,formals ,exp-list ...)
        (let ([formal-k (gensym)])
          (k `(lambda (,@formals ,formal-k)
                ,(transform `(begin ,@exp-list) (lambda (v) `(,formal-k ,v))))))]
      [`(if ,pred-exp ,then-exp ,else-exp)
        (transform pred-exp
                   (lambda (pred-v)
                     (let-fk k
                             (lambda (k)
                               `(if ,pred-v ,(transform then-exp k) ,(transform else-exp k))))))]
      [`(begin ,exp-list ...)
        (transform-list exp-list 
                        (lambda (value-list)
                          (k (last value-list))))]
      [`(call/cc ,f-exp)
        (transform f-exp
                   (lambda (f-v)
                     (let-fk k
                             (lambda (k)
                               (let ([formal-value (gensym)])
                                 `(,f-v (lambda (,formal-value _) ,(k formal-value)) ,(fk->expk k)))))))]
      [`(,p-exp ,exp-list ...)
        (transform p-exp
                   (lambda (p-v)
                     (transform-list 
                       exp-list
                       (lambda (value-list)
                         (let ([value-list (map cps-of-builtin value-list)])
                           (cond
                             [(memq p-v pure-builtins) (k `(,p-v ,@value-list))]
                             [(memq p-v side-effect-builtins) 
                              (let ([expk (k '(void))])
                                (if (equal? expk '(void))
                                  `(,p-v ,@value-list)
                                  `(begin (,p-v ,@value-list) ,expk)))]
                             [else 
                               `(,p-v ,@value-list ,(fk->expk k))]))))))])
    )
  (transform e identity)
  )
;------------------------------
(define (expand-library-forms e)
  (match 
    e
    [(? not-pair? e) e]
    [`(define (,name ,formal-list ...) ,exp-list ...) 
      (expand-library-forms `(define ,name (lambda ,formal-list ,@exp-list)))]
    [`(define ,(? symbol? name) (,(? (lambda (x) (not (memq x '(lambda quote)))) form) ,exp-list ...))
      `(begin (define ,name 'undefined) (set! ,name (,form ,@exp-list)))]
    [`(let ,(? symbol? fname) ,name-values ,exp-list ...) 
      (expand-library-forms `((lambda ()
                                (define ,fname (lambda ,(map car name-values) ,@exp-list))
                                (,fname ,@(map cadr name-values)))))]
    [`(let ,name-values ,exp-list ...) 
      (expand-library-forms `((lambda ,(map car name-values) ,@exp-list)
                              ,@(map cadr name-values)))]
    [`(let* ,name-values ,exp-list ...) 
      (expand-library-forms (if (= (length name-values) 1)
                              `(let ,name-values ,@exp-list)
                              `(let (,(car name-values)) (let* ,(cdr name-values) ,@exp-list))))]
    [`(letrec ,name-values ,exp-list ...) 
      (expand-library-forms `((lambda ,(map car name-values)
                                ,@(map (lambda (name-value) `(set! ,(car name-value) ,(cadr name-value))) name-values)
                                ,@exp-list)
                              ,@(map (lambda (_) ''undefined) name-values)))]
    [`(cond ,case-list ...) 
      (expand-library-forms (if (= 1 (length case-list))
                              `(begin ,@(cdar case-list))
                              `(if ,(caar case-list) (begin ,@(cdar case-list)) (cond ,@(cdr case-list)))))]
    [`(and ,exp-list ...) 
      (expand-library-forms (if (= 1 (length exp-list)) 
                              (car exp-list)
                              `(if ,(car exp-list) (and ,@(cdr exp-list)) false)))]
    [`(or ,exp-list ...) 
      (expand-library-forms (if (= 1 (length exp-list)) 
                              (car exp-list)
                              (let ([v (gensym)])
                                `(let ([,v ,(car exp-list)]) (if ,v ,v (or ,@(cdr exp-list)))))))]
    [else 
      (cons (expand-library-forms (car e)) (expand-library-forms (cdr e)))])
  )
;------------------------------
(define (find-symbols symbols e)
  (cond
    [(memq e symbols) => (compose list car)]
    [(not-pair? e) empty]
    [else (append (find-symbols symbols (car e)) (find-symbols symbols (cdr e)))])
  )
(define (make-builtin-tracker)
  (let* ([cps-to-ds (map (lambda (name) (list (cps-of-builtin name) name))
                         (append pure-builtins side-effect-builtins))]
         [cps-list (map car cps-to-ds)]
         [tracked-cps (list->set empty)]
         [print-cps 
           (lambda (cps-ds)
             (pretty-print `(define ,(car cps-ds) 
                              (lambda args 
                                ((last args) (apply ,(cadr cps-ds) (drop-right args 1)))))))])
    (lambda (exp)
      (let ([found-cps (list->set (find-symbols cps-list exp))])
        (set-for-each (set-subtract found-cps tracked-cps) (lambda (name) (print-cps (assq name cps-to-ds))))
        (set! tracked-cps (set-union found-cps tracked-cps))))
    )
  )

(do ([datum (read) (read)][tracker (make-builtin-tracker)])
  ((eof-object? datum))
  (let ([exp (cps (expand-library-forms datum))])
    (tracker exp)
    (pretty-print exp))
  )
(flush-output)
