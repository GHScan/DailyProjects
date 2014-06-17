#lang racket

(provide (all-defined-out))
;------------------------------
(define pure-builtins '(
                        + - * / % quotient remainder expt
                        = < <= > >= not
                        add1 sub1 zero? identity sqr sqrt
                        cons car cdr eq? equal? eqv? empty? null?
                        drop length append reverse map filter foldl build-list for-each sort
                        current-inexact-milliseconds random eval
                        ))
(define side-effect-builtins '(
                               printf pretty-print print display exit
                               define set!
                               ))
;------------------------------
(define (atom? a)
  (not (or (pair? a) (empty? a)))
  )
;------------------------------
(define (cps e)
  (define (transform-list exp-list k)
    (let iter ([exp-list exp-list][ctx-list empty][v-list empty])
      (if (empty? exp-list)
        (k (reverse ctx-list) (reverse v-list))
        (transform (car exp-list) 
                   (lambda (ctx v)
                     (iter (cdr exp-list) (cons ctx ctx-list) (cons v v-list))))))
    )
  (define (build-cascaded-ctx ctx-list)
    (foldl (lambda (ctx init) (lambda (r) (init (ctx r)))) (car ctx-list) (cdr ctx-list))
    )
  (define (remove-alias-k e)
    (match e
           [(? atom? x) x]
           [`(lambda (,x) (,y ,x)) y]
           [`(,exp-list ...) (map remove-alias-k exp-list)])
    )
  (define (let-k exp k)
    (let ([exp (remove-alias-k exp)])
      (match exp
             [(? symbol? x) (k (lambda (_) _) (lambda (arg) `(,exp ,arg)))]
             [`(lambda (,y) ,y) (k (lambda (_) _) (lambda (arg) arg))]
             [else 
               (let ([v (gensym)])
                 (k (lambda (r) `(let ([,v ,exp]) ,r)) (lambda (arg) `(,v ,arg))))]))
    )
  (define (transform e k)
    (match 
      e
      [(? atom? x) 
       (k (lambda (_) _) x)]
      [`(quote ,exp)
        (k (lambda (_) _) e)]
      [`(lambda ,formals ,exp-list ...)
        (k (lambda (_) _) 
           (transform (cons 'begin exp-list)
                      (lambda (ctx v)
                        (let ([k2 (gensym)])
                          `(lambda (,@formals ,k2) ,(ctx `(,k2 ,v)))))))]
      [`(if ,exp-list ...) 
        (transform-list 
          exp-list 
          (lambda (ctx-list v-list)
            (let ([pred-v (car v-list)][then-v (cadr v-list)][else-v (caddr v-list)]
                  [pred-ctx (car ctx-list)][then-ctx (cadr ctx-list)][else-ctx (caddr ctx-list)]
                  [v (gensym)])
              (k (lambda (r)
                   (let-k `(lambda (,v) ,r) 
                          (lambda (let-ctx let-f)
                            (pred-ctx (let-ctx `(if ,pred-v ,(then-ctx (let-f then-v)) ,(else-ctx (let-f else-v))))))))
                 v))))]
      [`(begin ,exp-list ...) 
        (transform-list exp-list 
                        (lambda (ctx-list v-list)
                          (k (build-cascaded-ctx ctx-list) (last v-list))))]
      [`(call/cc ,f-exp)
        (transform 
          f-exp
          (lambda (f-ctx f-v)
            (let ([v (gensym)][t1 (gensym)][f-arg-arg (gensym)][f-arg-k (gensym)])
              (k (lambda (r) 
                   (let-k `(lambda (,v) ,r)
                          (lambda (let-ctx let-f)
                            (f-ctx (let-ctx `(,f-v 
                                               (lambda (,f-arg-arg ,f-arg-k) ,(let-f f-arg-arg)) 
                                               (lambda (,t1) ,(let-f t1)))))))) 
                 v))))]
      [`(,exp-list ...) 
        (transform-list exp-list
                        (lambda (ctx-list v-list)
                          (let ([ctx (build-cascaded-ctx ctx-list)])
                            (cond
                              [(memq (car v-list) pure-builtins) 
                               (k ctx v-list)]
                              [(memq (car v-list) side-effect-builtins) 
                               (k (lambda (r) 
                                    (ctx (if (equal? r '(void))
                                           v-list
                                           `(begin ,v-list ,r)))) 
                                  '(void))]
                              [else 
                                (let ([v (gensym)])
                                  (k (lambda (r) (ctx `(,@v-list (lambda (,v) ,r)))) v))]))))])
    )

  (remove-alias-k (transform e (lambda (ctx v) (ctx v))))
  )
;------------------------------
(define (expand-library-forms e)
  (match 
    e
    [(? atom? e) e]
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
      (map expand-library-forms e)])
  )
;------------------------------
(do ([datum (read) (read)])
  ((eof-object? datum))
  (pretty-print (cps (expand-library-forms datum))))
(flush-output)
