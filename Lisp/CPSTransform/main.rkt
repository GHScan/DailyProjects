#lang racket

(provide (all-defined-out))
;------------------------------
(define ds-builtins '(
                      + - * / % quotient remainder expt
                      = < <= > >= not
                      add1 sub1 zero? identity sqr sqrt
                      cons car cdr eq? equal? eqv? empty? null?
                      drop length append reverse map filter foldl build-list for-each
                      current-inexact-milliseconds random eval
                      ))
(define cps-builtins '(
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
                   (let ([branch-k (remove-alias-k `(lambda (,v) ,r))])
                     (match branch-k
                            [(? atom? x) (pred-ctx `(if ,pred-v ,(then-ctx `(,x ,then-v)) ,(else-ctx `(,x ,else-v))))]
                            [`(lambda (,y) ,y) (pred-ctx `(if ,pred-v ,(then-ctx then-v) ,(else-ctx else-v)))]
                            [else (let ([branch-v (gensym)])
                                    (pred-ctx `(let ([,branch-v ,branch-k])
                                                 (if ,pred-v ,(then-ctx `(,branch-v ,then-v)) ,(else-ctx `(,branch-v ,else-v))))))])))
                 v))))]
      [`(begin ,exp-list ...) 
        (transform-list exp-list 
                        (lambda (ctx-list v-list)
                          (k (build-cascaded-ctx ctx-list) (last v-list))))]
      [`(,exp-list ...) 
        (transform-list exp-list
                        (lambda (ctx-list v-list)
                          (if (memq (car v-list) ds-builtins)
                            (k (build-cascaded-ctx ctx-list) v-list)
                            (let ([v (gensym)])
                              (k (build-cascaded-ctx 
                                   (append ctx-list (list (lambda (r) `(,@v-list (lambda (,v) ,r))))))
                                 v)))))])
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
