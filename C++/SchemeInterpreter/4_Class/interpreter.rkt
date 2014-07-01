#lang racket

(require "utils.rkt")
;------------------------------
(define the-constants empty)
(define the-globals empty)
(define the-funcs empty)
(define the-classes empty)
(define the-global-map empty)
(define the-main-func empty)
;------------------------------
(define (build-func source)
  (match 
    source
    [`((eval-stack-size ,eval-stack-size)
       (symbols ,symbols)
       (frees ,frees)
       (codes ,codes))
      (build-label-map 
        codes
        (lambda (labels codes)
          (list (list->vector (map cadr frees)) labels codes)))])
  )

(define (build-class source)
  (match 
    source
    [`((fields ,fields)
       (methods ,methods)
       (funcs ,funcs))
      (let ([name->field-index (map (lambda (p) (cons (cadr p) (car p))) fields)]
            [name->method-index (map (lambda (p) (cons (cadr p) (car p))) methods)]
            [method-vec (list->vector (map (lambda (func) (vector-ref the-funcs (cadr func))) funcs))])
        (list name->field-index name->method-index method-vec))])
  )

(define (load-source)
  (let ([source
          (do ([line (read) (read)][source empty (cons line source)])
            ((eof-object? line) (reverse source)))])
    (match source
           [`(((main-func ,main-index)
               (constants ,constants)
               (globals ,globals)
               (classes ,classes)
               (funcs ,funcs)))
             (set! the-constants (list->vector (map cadr constants)))
             (set! the-globals (list->vector (map cadr globals)))
             (set! the-funcs (list->vector (map (lambda (index-func) (build-func (cadr index-func))) funcs)))
             (set! the-classes (list->vector (map (lambda (index-class) (build-class (cadr index-class))) classes)))
             (set! the-main-func (vector-ref the-funcs main-index))
             (set! the-global-map (map (lambda (v) (cons (cadr v) (car v))) globals))]))
  )

;------------------------------
(define builtins (list
                   (cons '+ +) (cons '- -) (cons '* *) (cons '/ /) (cons 'quotient quotient) (cons 'remainder remainder)
                   (cons 'sqr sqr) (cons 'sqrt sqrt) (cons 'identity identity)
                   (cons 'true true) (cons 'false false) (cons 'else true)
                   (cons '= =) (cons 'not not) (cons '< <) (cons '<= <=) (cons '> >) (cons '>= >=) (cons 'eq? eq?)
                   (cons 'cons cons) (cons 'car car) (cons 'cdr cdr)
                   (cons 'drop drop) (cons 'append append) (cons 'length length) (cons 'empty empty) (cons 'empty? empty?)
                   (cons 'pretty-print pretty-print) (cons 'current-inexact-milliseconds current-inexact-milliseconds)
                   (cons 'random random)))

(define (setup-builtin)
  (for-each (lambda (kv)
              (let ([pos (assq (car kv) the-global-map)])
                (if pos
                  (vector-set! the-globals (cdr pos) (cdr kv))
                  'ok))) builtins)
  )

;------------------------------
(define (make-env prev-env actual-args)
  (list->vector (cons prev-env actual-args))
  )

(define (make-closure env func)
  (list env func)
  )

;------------------------------
(define the-class-closure-type-tag '(class-closure))

(define (make-class-closure env class)
  (list->vector (list env the-class-closure-type-tag class))
  )

(define (class-closure? o)
  (and (vector? o) (>= (vector-length o) 2) (eq? the-class-closure-type-tag (vector-ref o 1)))
  )

(define (apply-class-closure cclosure actual-args)
  (make-env cclosure actual-args)
  )

(define (get-field obj name)
  (let* ([cclosure (vector-ref obj 0)]
         [name->field-index (car (vector-ref cclosure 2))])
    (vector-ref obj (add1 (cdr (assq name name->field-index)))))
  )

(define (get-method-func obj name)
  (let* ([class (vector-ref (vector-ref obj 0) 2)]
         [name->method-index (cadr class)])
    (vector-ref (caddr class) (cdr (assq name name->method-index))))
  )

(define (get-method-func-by-index cclosure index)
  (let* ([class (vector-ref cclosure 2)])
    (vector-ref (caddr class) index))
  )

;------------------------------
(define (get-free-env env env-index)
  (if (zero? env-index)
    env
    (get-free-env (vector-ref env 0) (sub1 env-index)))
  )

(define (interpret eval-stack codes labels frees env)
  (if (empty? codes)
    (car eval-stack)
    (match
      (car codes)
      [`(loadk ,index) 
        (interpret (cons (vector-ref the-constants index) eval-stack) (cdr codes) labels frees env)]
      [`(loadvar (global ,index)) 
        (interpret (cons (vector-ref the-globals index) eval-stack) (cdr codes) labels frees env)]
      [`(loadvar (local ,index)) 
        (interpret (cons (vector-ref env (add1 index)) eval-stack) (cdr codes) labels frees env)]
      [`(loadvar (free ,index)) 
        (let* ([address (vector-ref frees index)]
               [v (vector-ref (get-free-env env (car address)) (add1 (cadr address)))])
          (interpret (cons v eval-stack) (cdr codes) labels frees env))]
      [`(storevar (global ,index)) 
        (vector-set! the-globals index (car eval-stack))
        (interpret (cdr eval-stack) (cdr codes) labels frees env)]
      [`(storevar (local ,index)) 
        (vector-set! env (add1 index) (car eval-stack))
        (interpret (cdr eval-stack) (cdr codes) labels frees env)]
      [`(storevar (free ,index)) 
        (let ([address (vector-ref frees index)])
          (vector-set! (get-free-env env (car address)) (add1 (cadr address)) (car eval-stack))
          (interpret (cdr eval-stack) (cdr codes) labels frees env))]
      [`(loadfunc ,index) 
        (interpret (cons (make-closure env (vector-ref the-funcs index)) eval-stack) (cdr codes) labels frees env)]
      [`(pop)
        (interpret (cdr eval-stack) (cdr codes) labels frees env)]
      [`(jmp ,label)
        (interpret eval-stack (cdr (assq label labels)) labels frees env)]
      [`(tjmp ,label)
        (if (car eval-stack)
          (interpret (cdr eval-stack) (cdr (assq label labels)) labels frees env)
          (interpret (cdr eval-stack) (cdr codes) labels frees env))]
      [`(tail) 
        (if (eq? 'call (caadr codes))
          (let* ([actual-count (cadadr codes)]
                 [actuals-and-func (take eval-stack (add1 actual-count))])
            (apply-function (last actuals-and-func) (reverse (drop-right actuals-and-func 1))))
          (let* ([actual-count (caddr (cadr codes))]
                 [op (cadadr codes)]
                 [v (apply (cdr (assq op builtins)) (reverse (take eval-stack actual-count)))])
            v))]
      [`(call ,actual-count)
        (let* ([actuals-and-func (take eval-stack (add1 actual-count))]
               [v (apply-function (last actuals-and-func) (reverse (drop-right actuals-and-func 1)))])
          (interpret (cons v (drop eval-stack (add1 actual-count))) (cdr codes) labels frees env))]
      [`(loadclass ,classindex)
        (let ([cclosure (make-class-closure env (vector-ref the-classes classindex))])
          (interpret (cons cclosure eval-stack) (cdr codes) labels frees env))]
      [`(getfield (k ,index))
        (let ([v (get-field (car eval-stack) (vector-ref the-constants index))])
          (interpret (cons v (cdr eval-stack)) (cdr codes) labels frees env))]
      [`(getmethod (k ,index))
        (let* ([func (get-method-func (car eval-stack) (vector-ref the-constants index))]
              [v (make-closure (car eval-stack) func)])
         (interpret (cons v (cdr eval-stack)) (cdr codes) labels frees env))]
      [`(loadvar (,free-method ,index))
        (let* ([address (vector-ref frees index)]
               [func (get-method-func-by-index (get-free-env env (car address)) (cadr address))]
               [v (make-closure (get-free-env env (sub1 (car address))) func)])
          (interpret (cons v eval-stack) (cdr codes) labels frees env))]
      [`(inline-op ,(? symbol? op) ,actual-count)
        (let* ([f (cdr (assq op builtins))]
               [actual-args (reverse (take eval-stack actual-count))]
               [v (apply f actual-args)])
          (interpret (cons v (drop eval-stack actual-count)) (cdr codes) labels frees env))]
      [else (error "invalid message")]))
  )

(define (apply-function func actual-args)
  (match 
    func
    [(? procedure? x) (apply x actual-args)]
    [(? class-closure? x) (apply-class-closure x actual-args)]
    [`(,prev-env (,frees ,labels ,codes))
      (interpret empty codes labels frees (make-env prev-env actual-args))])
  )

;------------------------------
(load-source)
(setup-builtin)
(apply-function (make-closure empty the-main-func) empty)
