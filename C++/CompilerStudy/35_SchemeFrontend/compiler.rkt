#lang racket

;------------------------------
(define (flatten-map f l)
  (apply append (map f l))
  )

;------------------------------
(define (remove-defines exp k)
  (match 
    exp 
    [(? (compose not pair?) x) (k x empty)]
    [`(define ,name ,right-exp)
      (remove-defines right-exp 
                      (lambda (right-exp vars)
                        (k `(set! ,name ,right-exp) (cons name vars))))]
    [`(lambda ,formals ,body-exp)
      (remove-defines body-exp 
                      (lambda (body-exp vars)
                        (if (empty? vars) 
                          (k `(lambda ,formals ,body-exp) empty)
                          (k `(lambda ,formals 
                                ((lambda ,vars ,body-exp) 
                                 ,@(map (lambda (_) 'undefined) vars))) 
                             empty))))]
    [else 
      (let iter ([exp-list exp][new-exp-list empty][vars empty])
        (if (empty? exp-list)
          (k (reverse new-exp-list) vars)
          (remove-defines (car exp-list)
                          (lambda (first-exp first-vars)
                            (iter (cdr exp-list) (cons first-exp new-exp-list) (append first-vars vars))))))])
  )

(define (expand-library-forms e)
  (match 
    e
    [(? (lambda (x) (not (pair? x)))) e]
    [`(define (,name ,formal-list ...) ,exp-list ...) 
      (expand-library-forms `(define ,name (lambda ,formal-list ,@exp-list)))]
    [`(lambda ,formals ,exp-list ...)
      `(lambda ,formals 
         ,(expand-library-forms (if (= 1 (length exp-list)) (car exp-list) (cons 'begin exp-list))))]
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
(define (make-index-table)
  (letrec ([table (make-hash)]
           [self
             (lambda (m . args)
               (cond
                 [(eq? m 'has?) (hash-has-key? table (car args))]
                 [(eq? m 'get) (hash-ref table (car args))]
                 [(eq? m 'get-or-add) (self 'add (car args)) (self 'get (car args))]
                 [(eq? m 'add-new) 
                  (if (self 'has? (car args))
                    (void)
                    (hash-set! table (car args) (hash-count table)))]
                 [(eq? m 'add)
                  (if (self 'has? (car args))
                    'ok
                    (self 'add-new (car args)))
                  (self 'get (car args))]
                 [(eq? m 'keys) 
                  (map car (sort (hash->list table) (lambda (a b) (< (cdr a) (cdr b)))))]
                 [(eq? m 'tolist) 
                  (map (lambda (p) (list (cdr p) (car p))) 
                       (sort (hash->list table) (lambda (a b) (< (cdr a) (cdr b)))))]
                 [else (error "invalid message:" m)]))])
    self)
  )

(define (make-symbol-table prev-table init-symbols)
  (letrec ([symbols (make-index-table)]
           [frees (make-index-table)]
           [lookup 
             (lambda (sym)
               (let search ([table-index 0][table self])
                 (if (or (table 'global?) (table 'has? sym))
                   (cond
                     [(table 'global?) `(global ,((table '**symbols) 'get-or-add sym))]
                     [(= 0 table-index) `(local ,((table '**symbols) 'get sym))]
                     [else `(free ,(frees 'get-or-add (cons table-index ((table '**symbols) 'get sym))))])
                   (search (+ table-index 1) (table 'prev-table)))))]
           [define-
             (lambda (sym)
               (if (self 'global?)
                 (symbols 'add sym)
                 (symbols 'add-new sym)))]
           [self 
             (lambda (m . args)
               (cond
                 [(eq? m 'global?) (empty? prev-table)]
                 [(eq? m 'lookup) (lookup (car args))]
                 [(eq? m 'define) (define- (car args))]
                 [(eq? m 'has?) (symbols 'has? (car args))]
                 [(eq? m 'prev-table) prev-table]
                 [(eq? m 'symbols) (symbols 'tolist)]
                 [(eq? m 'frees) (frees 'tolist)]
                 [(eq? m '**symbols) symbols]
                 [else (error "invalid message")]))])
    (for-each (curry self 'define) init-symbols)
    self)
  )


;------------------------------
(define (optimize-pass-remove-duplicate-pop codes)
  (cond
    [(or (empty? codes) (empty? (cdr codes))) codes]
    [(and (memq (caar codes) '(loadk loadvar loadfunc)) (eq? (caadr codes) 'pop)) 
     (optimize-pass-remove-duplicate-pop (cddr codes))]
    [else (cons (car codes) (optimize-pass-remove-duplicate-pop (cdr codes)))])
  )

(define (optimize codes)
  (let ([pass-list (list optimize-pass-remove-duplicate-pop)])
    (foldl (lambda (pass codes) (pass codes)) codes pass-list))
  )

;------------------------------
(define (make-func sym-table codes)
  (let ([codes (optimize codes)])
    `((symbols ,(sym-table 'symbols))
      (frees ,(sym-table 'frees))
      (codes ,codes)))
  )

(define the-constants (make-index-table))
(define the-funcs (make-index-table))

(define (eval sym-table tail exp)
  (match 
    exp
    [(? (lambda (x) (or (string? x) (number? x) (boolean? x))) x) 
     `((loadk ,(the-constants 'get-or-add x)))]
    [(? symbol? x)
     `((loadvar ,(sym-table 'lookup x)))]
    [(? (compose not pair?) x) 
     (error "invalid exp:" x)]
    [`(if ,pred-exp ,then-exp ,else-exp)
      (let ([label-then (gensym)][label-end (gensym)])
        `(,@(eval sym-table false pred-exp)
           (tjmp ,label-then)
           ,@(eval sym-table tail else-exp)
           (jmp ,label-end)
           (label ,label-then)
           ,@(eval sym-table tail then-exp)
           (label ,label-end)))]
    [`(lambda ,formals ,body-exp)
      (let* ([new-sym-table (make-symbol-table sym-table formals)]
             [codes (eval new-sym-table true body-exp)]
             [findex (the-funcs 'get-or-add (make-func new-sym-table codes))])
        `((loadfunc ,findex)))]
    [`(begin ,exp-list ...)
      `(,@(flatten-map
            (lambda (e)
              `(,@(eval sym-table false e) (pop)))
            (drop-right exp-list 1))
         ,@(eval sym-table tail (last exp-list)))]
    [`(set! ,(? symbol? name) ,right-exp)
      `(,@(eval sym-table false right-exp)
         (storevar ,(sym-table 'lookup name))
         (loadk ,(the-constants 'get-or-add 0)))]
    [`(,op ,actuals ...)
      `(,@(eval sym-table false op)
         ,@(flatten-map (curry eval sym-table false) actuals)
         ,@(if tail '((tail)) empty)
         (call ,(length actuals)))])
  )

;------------------------------
(define (preprocess e)
  (remove-defines (expand-library-forms e) (lambda (e _) e))
  )

(define source 
  (do ([line (read) (read)]
       [source empty (cons (preprocess line) source)])
    ((eof-object? line) (reverse source))))

;(pretty-print source)

(pretty-print
  (let* ([g-sym-table (make-symbol-table empty empty)]
         [codes (eval g-sym-table true (cons 'begin source))]
         [main-index (the-funcs 'get-or-add (make-func (make-symbol-table empty empty) codes))])
    `((main-func ,main-index)
      (constants ,(the-constants 'tolist))
      (globals ,(g-sym-table 'symbols))
      (funcs ,(the-funcs 'tolist)))))
