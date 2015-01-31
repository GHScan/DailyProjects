#lang racket

(define-syntax letv*
  (syntax-rules 
    ()
    [(_ () body ...) (begin body ...)]
    [(_ ([x0 v0] [x1 v1] ...) body ...)
     (let-values ([x0 v0])
                 (letv* ([x1 v1] ...)
                        body ...))]))

(define (print-list l)
  (pretty-print `((length ,(length l)) ,l)))

(define (flat-map f l)
  (apply append (map f l)))

(define (lookup k env)
  (cond
    [(assq k env) => cdr]
    [else #f]))

(define (updated k v env)
  (cond
    [(empty? env) '()]
    [(eq? (caar env) k) (cons (cons k v) (cdr env))]
    [else (cons (car env) (updated k v (cdr env)))]))

(define (remq-key k env)
  (remove k env (lambda (a b) (eq? a (car b)))))

(define (make-vregister-allocator)
  (let ([n 0])
    (lambda () 
      (set! n (+ n 1))
      `(VREG ,n))))
(define (vregister? a)
  (and (pair? a) (eq? 'VREG (car a))))

(define (make-register-allocator register-count temp-count)
  (letrec ([free-registers (build-list register-count (lambda (i) `(REG ,i)))]
           [register? (lambda (x) (and (pair? x) (eq? (car x) 'REG)))]
           [free-temps (build-list temp-count (lambda (i) `(TEMP ,i)))]
           [alloc-temp (lambda () 
                         (let ([temp (car free-temps)]) 
                           (set! free-temps (cdr free-temps)) 
                           temp))]
           [free-temp (lambda (temp) 
                        (set! free-temps (cons temp free-temps)))]
           [vreg-2-storage '()]
           [spilling
             (lambda ()
               (let* ([kv (findf (lambda (kv) (register? (cdr kv))) (reverse vreg-2-storage))]
                      [vreg (car kv)]
                      [reg (cdr kv)]
                      [temp (alloc-temp)])
                 (set! vreg-2-storage (updated vreg temp vreg-2-storage))
                 (values reg `((store ,temp ,reg)))))]
           [alloc-register
             (lambda ()
               (cond
                 [(empty? free-registers) (spilling)]
                 [else (let ([reg (car free-registers)])
                         (set! free-registers (cdr free-registers))
                         (values reg '()))]))]
           [free-register
             (lambda (reg)
               (set! free-registers (cons reg free-registers)))]
           [alloc 
             (lambda (vreg)
               (let-values ([(reg codes) (alloc-register)])
                           (set! vreg-2-storage (cons (cons vreg reg) vreg-2-storage))
                           (values reg codes)))]
           [free
             (lambda (vreg)
               (let ([storage (lookup vreg vreg-2-storage)])
                 (cond
                   [(register? storage) (free-register storage)]
                   [else (free-temp storage)])
                 (set! vreg-2-storage (remq-key vreg vreg-2-storage))))]
           [get 
             (lambda (vreg)
               (let ([storage (lookup vreg vreg-2-storage)]
                     [vreg-2-storage2 (remq-key vreg vreg-2-storage)])
                 (cond
                   [(register? storage) 
                    (set! vreg-2-storage (cons (cons vreg storage) vreg-2-storage2))
                    (values storage '())]
                   [else 
                     (let-values ([(reg codes) (alloc-register)])
                                 (let ([codes `(,@codes (load ,reg ,storage))])
                                   (free-temp storage)
                                   (set! vreg-2-storage (cons (cons vreg reg) vreg-2-storage2))
                                   (values reg codes)))])))])
    (lambda (cmd . args)
      (cond
        [(eq? cmd 'alloc) (apply alloc args)]
        [(eq? cmd 'free) (apply free args)]
        [(eq? cmd 'get) (apply get args)]))))

(define (expr-to-1address-code stmts)
  (letrec ([process-expr 
             (lambda (e)
               (match 
                 e
                 [(? number? n) `((loadi ,n))]
                 [(? symbol? a) `((load ,a))]
                 [`(,var = ,e) `(,@(process-expr e) (pop ,var))]
                 [`(,op ,a1 ,a2) `(,@(process-expr a2) ,@(process-expr a1) (,op))]))])
    (flat-map process-expr stmts)))

(define (expr-to-2address-code stmts)
  (letrec ([alloc-vregister (make-vregister-allocator)]
           [process-expr
             (lambda (e)
               (match 
                 e
                 [(? number? n) (let ([g (alloc-vregister)]) (values g `((loadi ,g ,n))))]
                 [(? symbol? a) (let ([g (alloc-vregister)]) (values g `((load ,g ,a))))]
                 [`(,var = ,e) 
                   (let-values ([(r c) (process-expr e)]) 
                               (values var `(,@c (store ,var ,r))))]
                 [`(,op ,a1 ,a2) 
                   (let-values ([(r1 c1) (process-expr a1)]
                                [(r2 c2) (process-expr a2)])
                               (values r1 `(,@c1 ,@c2 (,op ,r1 ,r2))))]))]
           [process-stmt
             (lambda (stmt)
               (let-values ([(r c) (process-expr stmt)])
                           c))])
    (flat-map process-stmt stmts)))

(define (expr-to-3address-code stmts)
  (letrec ([alloc-vregister (make-vregister-allocator)]
           [process-expr
             (lambda (e)
               (match 
                 e
                 [(? number? n) (let ([g (alloc-vregister)]) (values g `((loadi ,g ,n))))]
                 [(? symbol? a) (let ([g (alloc-vregister)]) (values g `((load ,g ,a))))]
                 [`(,var = ,e) 
                   (let-values ([(r c) (process-expr e)]) 
                               (values var `(,@c (store ,var ,r))))]
                 [`(,op ,a1 ,a2) 
                   (let-values ([(r2 c2) (process-expr a2)]
                                [(r1 c1) (process-expr a1)])
                               (let ([g (alloc-vregister)]) (values g `(,@c2 ,@c1 (,op ,g ,r1 ,r2)))))]))]
           [process-stmt
             (lambda (stmt)
               (let-values ([(r c) (process-expr stmt)])
                           c))])
    (flat-map process-stmt stmts)))

(define (_1address-to-3address-code codes)
  (letrec ([alloc-vregister (make-vregister-allocator)]
           [simulate 
             (lambda (codes stack)
               (match
                 codes
                 ['() '()]
                 [`((loadi ,n) . ,codes) (let ([g (alloc-vregister)]) (cons `(loadi ,g ,n) (simulate codes (cons g stack))))]
                 [`((load ,a) . ,codes) (let ([g (alloc-vregister)]) (cons `(load ,g ,a) (simulate  codes (cons g stack))))]
                 [`((pop ,a) . ,codes) (cons `(store ,a ,(car stack)) (simulate  codes (cdr stack)))]
                 [`((,op) . ,codes) (let ([g (alloc-vregister)]) (cons `(,op ,g ,(car stack) ,(cadr stack)) (simulate codes (cons g (cddr stack)))))]))])
    (simulate codes '())))

(define (compact-3address-code codes)
  (letrec ([redirect 
             (lambda (code target)
               (match 
                 code
                 [`(loadi ,g ,n) `(loadi ,target ,n)]
                 [`(load ,g ,a) `(load ,target ,a)]
                 [`(,op ,g ,a ,b) `(,op ,target ,a ,b)]))]
           [get-operand 
             (lambda (activeRegisters reg)
               (match 
                 (lookup reg activeRegisters)
                 [`(loadi ,g ,n) (values n (remq-key reg activeRegisters))]
                 [`(load ,g ,a) (values a (remq-key reg activeRegisters))]
                 [`(,op ,target ,a ,b) (values target activeRegisters)]))]
           [rewrite
             (lambda (codes activeRegisters)
               (match 
                 codes
                 ['() (values activeRegisters '())]
                 [`((loadi ,g ,n) . ,codes2) (rewrite codes2 (cons (cons g (car codes)) activeRegisters))]
                 [`((load ,g ,a) . ,codes2) (rewrite codes2 (cons (cons g (car codes)) activeRegisters))]
                 [`((store ,a ,g) . ,codes2) 
                   (let-values ([(activeRegisters2 codes3) (rewrite codes2 (remq-key g activeRegisters))])
                               (values activeRegisters2 (cons (redirect (lookup g activeRegisters) a) codes3)))]
                 [`((,op ,target ,a ,b) . ,codes2) 
                   (letv* ([(a activeRegisters) (get-operand activeRegisters a)]
                           [(b activeRegisters) (get-operand activeRegisters b)]
                           [(activeRegisters) (values (cons (cons target `(,op ,target ,a ,b)) activeRegisters))]
                           [(activeRegisters2 codes3) (rewrite codes2 activeRegisters)])
                          (values 
                            activeRegisters2 
                            (cond
                              [(lookup target activeRegisters2) => (lambda (code) (cons code codes3))]
                              [else codes3])))]))])
    (let-values ([(activeRegisters codes) (rewrite codes '())])
                codes)))


(define (register-renaming-for-3address-code codes)
  (letrec ([allocator (make-register-allocator 2 16)]
           [renaming-target
             (lambda (a) 
               (cond
                 [(vregister? a) 
                  (allocator 'alloc a)]
                 [else (values a '())]))]
           [renaming-ref
             (lambda (a)
               (cond
                 [(vregister? a) 
                  (let-values ([(reg codes) (allocator 'get a)])
                              (allocator 'free a)
                              (values reg codes))]
                 [else (values a '())]))]
           [process 
             (lambda (codes)
               (match
                 codes
                 ['() '()]
                 [`((,op ,target ,a ,b) . ,codes2)
                   (letv* ([(rb codesb) (renaming-ref b)]
                           [(ra codesa) (renaming-ref a)]
                           [(rt codest) (renaming-target target)])
                          (append `(,@codesb ,@codesa ,@codest (,op ,rt ,ra ,rb)) (process codes2)))]
                 [`((loadi ,target ,n) . ,codes2) 
                   (cons (car codes) (process codes2))]
                 [`((load ,target ,a) . ,codes2) 
                   (let-values ([(ra codesa) (renaming-ref a)])
                               (append `(,@codesa (load ,target ,ra)) (process codes2)))]))])
    (process codes)))

(define expr '((a = 3)
               (b = a)
               (c = (+ a b))
               (d = (- (+ (/ (* 3 (+ 1 c)) (% b (+ 8 a))) (* (* 1 (+ b a)) (/ 4 (+ 3 c)))) (* (+ (* 8 (- a b)) (- a (+ a b))) (- (* 8 (+ 8 c)) (/ 4 (* a a))))))))

(print-list (expr-to-1address-code expr))
(print-list (expr-to-2address-code expr))
(print-list (expr-to-3address-code expr))
(print-list (_1address-to-3address-code (expr-to-1address-code expr)))
(print-list (compact-3address-code (expr-to-3address-code expr)))
(print-list (register-renaming-for-3address-code (compact-3address-code (expr-to-3address-code expr))))
