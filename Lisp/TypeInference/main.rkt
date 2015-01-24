#lang racket

; Hindly-Milner style type inferencer for lambda calculus (without let-polymorphism)

(define-syntax letv*
  (syntax-rules 
    ()
    [(_ () body ...) (begin body ...)]
    [(_ ([x0 v0] [x1 v1] ...) body ...)
     (let-values ([x0 v0])
                 (letv* ([x1 v1] ...)
                        body ...))]))

(define (lookup k table) 
  (cond
    [(assq k table) => cdr]
    [else #f]))

(define (ext-table k v table)
  (cons (cons k v) table))

(define (builtin-type? t)
  (memq t '(int bool string)))

(define builtin-values-type 
  '((+ . (int -> (int -> int)))
    (- . (int -> (int -> int)))
    (* . (int -> (int -> int)))
    (/ . (int -> (int -> int)))
    (< . (int -> (int -> bool)))
    (<= . (int -> (int -> bool)))
    (> . (int -> (int -> bool)))
    (>= . (int -> (int -> bool)))
    (= . (int -> (int -> bool)))
    (zero? . (int -> bool))
    (add1 . (int -> int))
    (sub1 . (int -> int))
    (not . (bool -> bool))))

(define (type-variable? t)
  (and (symbol? t) (not (builtin-type? t))))

(define (symbol-less a b)
  (string<? (symbol->string a) (symbol->string b)))

(define (swap-if-less a b less)
  (if (less a b) (values a b) (values b a)))

(define (final-type t t->t2)
  (define (final-type-1 t t->t2 occurs)
    (match 
      t
      [`(,t1 -> ,t2) `(,(final-type-1 t1 t->t2 occurs) -> ,(final-type-1 t2 t->t2 occurs))]
      [_ (let ([t2 (lookup t t->t2)])
           (cond
             [(not t2) t]
             [(memq t2 occurs) (begin (pretty-print `("Found recursive type:" ,t2)) t2)]
             [else (final-type-1 t2 t->t2 (cons t2 occurs))]))]))
  (final-type-1 t t->t2 empty))

(define (unify t1 t2 t->t2)
  (match 
    `(,(final-type t1 t->t2) ,(final-type t2 t->t2))
    [`(,(? type-variable? t1) ,(? type-variable? t2)) 
      (letv* ([(t1 t2) (swap-if-less t1 t2 symbol-less)])
             (values (not (eq? t1 t2)) (ext-table t2 t1 t->t2)))]
    [`(,(? type-variable? t1) ,t2) (values #t (ext-table t1 t2 t->t2))]
    [`(,t1 ,(? type-variable? t2)) (values #t (ext-table t2 t1 t->t2))]
    [`(,(? builtin-type? t1) ,(? builtin-type? t2)) (values (eq? t1 t2) t->t2)]
    [`((,t11 -> ,t12) (,t21 -> ,t22)) 
      (letv* ([(v1 t->t2) (unify t11 t21 t->t2)]
              [(v2 t->t2) (unify t12 t22 t->t2)])
             (values (and v1 v2) t->t2))]
    [_ (values #f t->t2)]))

(define (infer v)
  (define (infer-1 v v->t t->t2)
    (match 
      v
      [(? boolean?) (values 'bool t->t2)]
      [(? number?) (values 'int t->t2)]
      [(? string?) (values 'string t->t2)]
      [(? symbol?) (values (lookup v v->t) t->t2)]
      [`(if ,test ,conseq ,alt) 
        (letv* ([(testt t->t2) (infer-1 test v->t t->t2)]
                [(u1 t->t2) (unify testt 'bool t->t2)]
                [(conseqt t->t2) (infer-1 conseq v->t t->t2)]
                [(altt t->t2) (infer-1 alt v->t t->t2)]
                [(u2 t->t2) (unify conseqt altt t->t2)])
               (cond
                 [(not u1) (values (pretty-print `("infer failed:" ,test"(",(final-type testt t->t2)")" "should be bool")) t->t2)]
                 [(not u2) (values (pretty-print `("infer failed:" ,conseq"(",(final-type conseqt t->t2)")" ,alt"(",(final-type altt t->t2)")" "should be same type")) t->t2) ]
                 [else (values altt t->t2)]))]
      [`(lambda (,formal) ,body) 
        (letv* ([(v->t) (ext-table formal (gensym) v->t)]
                [(bodyt t->t2) (infer-1 body v->t t->t2)])
               (values `(,(lookup formal v->t) -> ,bodyt) t->t2))]
      [`(,f ,actual)
        (letv* ([(ft t->t2) (infer-1 f v->t t->t2)]
                [(formalt resultt) (values (gensym) (gensym))]
                [(u1 t->t2) (unify ft `(,formalt -> ,resultt) t->t2)]
                [(actualt t->t2) (infer-1 actual v->t t->t2)]
                [(u2 t->t2) (unify formalt actualt t->t2)])
               (cond
                 [(not u1) (values (pretty-print `("infer failed: " ,f"(",(final-type ft t->t2)")" "should be a function")) t->t2)]
                 [(not u2) (values (pretty-print `("infer failed: " ,f"(",(final-type ft t->t2)")" ,actual"(",(final-type actualt t->t2)")" "application type mismatch")) t->t2)] 
                 [else (values resultt t->t2)]))]))
  (letv* ([(t t->t2) (infer-1 v builtin-values-type empty)])
         (final-type t t->t2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; tests ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; correct programs
(infer 1)
; => int

(infer #t)
; => bool

(infer '(lambda (v) v))
; => (t0 -> t0)

(infer '(lambda (f) (lambda (x) (f x))))
; => ((t0 -> t1) -> (t0 -> t1))

(infer '(lambda (f) (lambda (x) (f (f x)))))
; => ((t0 -> t0) -> (t0 -> t0))

(infer '((lambda (f) (lambda (x) (f (f x)))) add1))
; => (int -> int)

(infer '(if (zero? 1) #t #f))
; => bool

(infer '(lambda (f) (lambda (x) (f ((+ x) 1)))))
; => ((int -> t0) -> (int -> t0))

(infer '(lambda (m) (lambda (n) (lambda (f) (lambda (x) ((m (n f)) x))))))
; => ((t0 -> (t1 -> t2)) -> ((t3 -> t0) -> (t3 -> (t1 -> t2))))

(infer '((lambda (f) (f 1)) (lambda (v) v)))
; => int

(infer '(if (zero? 1) #f #t))
; => bool

(define S '(lambda (x) (lambda (y) (lambda (z) ((x z) (y z))))))
(define K '(lambda (x) (lambda (y) x)))

(infer S)
; => ((t0 -> (t1 -> t2)) -> ((t0 -> t1) -> (t0 -> t2)))

(infer `(,S ,K))
; => ((t0 -> t1) -> (t0 -> t0))

(infer `((,S ,K) ,K))
; => (t0 -> t0)

; incorrect programs
(infer '(lambda (f) (f f)))
;; infer: trying to apply function to wrong type argument:
;;  - function:      f
;;  - function type: (t0 -> t1)
;;  - expected type: t0
;;  - argument type: (t0 -> t1)
;;  - argument: f

(infer '(if (zero? 1) #t 1))
;; infer: branches of conditional must have the same type
;;  - expression:        (if (zero? 1) #t 1)
;;  - true branch type:  bool
;;  - false branch type: int

(infer '((lambda (x) ((+ 1) x)) "hello"))
;; infer: trying to apply function to wrong type argument:
;;  - function:      (lambda (x) ((+ 1) x))
;;  - function type: (int -> int)
;;  - expected type: int
;;  - argument type: string
;;  - argument: hello
