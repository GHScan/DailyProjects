#lang racket

(define the-constants empty)
(define the-globals empty)
(define the-funcs empty)
(define the-global-map empty)
(define the-main-func empty)

;------------------------------
(define (build-label-map codes k)
  (cond
    [(empty? codes) (k empty empty)]
    [(eq? 'label (caar codes)) 
     (build-label-map (cdr codes) 
                      (lambda (labels new-codes)
                        (k (cons (cons (cadar codes) new-codes) labels) new-codes)))]
    [else 
      (build-label-map (cdr codes)
                       (lambda (labels new-codes)
                         (k labels (cons (car codes) new-codes))))])
  )

(define (build-func source)
  (match 
    source
    [`((symbols ,symbols)
       (frees ,frees)
       (codes ,codes))
      (build-label-map 
        codes
        (lambda (labels codes)
          (list (list->vector (map cadr frees)) labels codes)))])
  )

(define (load-source)
  (let ([source
          (do ([line (read) (read)][source empty (cons line source)])
            ((eof-object? line) (reverse source)))])
    (match source
           [`(((main-func ,main-index)
               (constants ,constants)
               (globals ,globals)
               (funcs ,funcs)))
             (set! the-constants (list->vector (map cadr constants)))
             (set! the-globals (list->vector (map cadr globals)))
             (set! the-funcs (list->vector (map (lambda (index-func) (build-func (cadr index-func))) funcs)))
             (set! the-main-func (vector-ref the-funcs main-index))
             (set! the-global-map (map (lambda (v) (cons (cadr v) (car v))) globals))]))
  )

;------------------------------
(define (setup-builtin)
  (let ([builtins 
          (list
            (cons '+ +) (cons '- -) (cons '* *) (cons '/ /) (cons 'quotient quotient) (cons 'remainder remainder)
            (cons 'sqr sqr) (cons 'sqrt sqrt) (cons 'identity identity)
            (cons 'true true) (cons 'false false) (cons 'else true)
            (cons '= =) (cons 'not not) (cons '< <) (cons '<= <=) (cons '> >) (cons '>= >=) (cons 'eq? eq?)
            (cons 'cons cons) (cons 'car car) (cons 'cdr cdr)
            (cons 'drop drop) (cons 'append append) (cons 'length length) (cons 'empty empty) (cons 'empty? empty?)
            (cons 'pretty-print pretty-print) (cons 'current-inexact-milliseconds current-inexact-milliseconds)
            (cons 'random random))])
    (for-each (lambda (kv)
                (let ([pos (assq (car kv) the-global-map)])
                  (if pos
                    (vector-set! the-globals (cdr pos) (cdr kv))
                    'ok))) builtins))
  )

;------------------------------
(define (make-env prev-env actual-args)
  (list->vector (cons prev-env actual-args))
  )

(define (make-closure env func)
  (list env func)
  )

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
               [v (vector-ref (get-free-env env (car address)) (add1 (cdr address)))])
          (interpret (cons v eval-stack) (cdr codes) labels frees env))]
      [`(storevar (global ,index)) 
        (vector-set! the-globals index (car eval-stack))
        (interpret (cdr eval-stack) (cdr codes) labels frees env)]
      [`(storevar (local ,index)) 
        (vector-set! env (add1 index) (car eval-stack))
        (interpret (cdr eval-stack) (cdr codes) labels frees env)]
      [`(storevar (free ,index)) 
        (let ([address (vector-ref frees index)])
          (vector-set! (get-free-env env (car address)) (add1 (cdr address)) (car eval-stack))
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
        (let* ([actual-count (cadadr codes)]
              [actuals-and-func (take eval-stack (add1 actual-count))])
         (apply-function (last actuals-and-func) (reverse (drop-right actuals-and-func 1))))]
      [`(call ,actual-count)
        (let* ([actuals-and-func (take eval-stack (add1 actual-count))]
               [v (apply-function (last actuals-and-func) (reverse (drop-right actuals-and-func 1)))])
          (interpret (cons v (drop eval-stack (add1 actual-count))) (cdr codes) labels frees env))]
      [else (error "invalid message")]))
  )

(define (apply-function func actual-args)
  (match 
    func
    [(? procedure? x) (apply x actual-args)]
    [`(,prev-env (,frees ,labels ,codes))
      (interpret empty codes labels frees (make-env prev-env actual-args))])
  )

;------------------------------
(load-source)
(setup-builtin)
(apply-function (make-closure empty the-main-func) empty)
