#lang racket

; Hindley-Milner style type inferencer for lambda calculus (without let-polymorphism)

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

(define (final-type t tenv)
  (define (final-type-1 t tenv occurs)
    (match 
      t
      [`(,t1 -> ,t2) `(,(final-type-1 t1 tenv occurs) -> ,(final-type-1 t2 tenv occurs))]
      [_ (let ([t2 (lookup t tenv)])
           (cond
             [(not t2) t]
             [(memq t2 occurs) (begin (pretty-print `("Found recursive type:" ,t2)) t2)]
             [else (final-type-1 t2 tenv (cons t2 occurs))]))]))
  (final-type-1 t tenv empty))

(define (unify t1 t2 tenv)
  (match 
    `(,(final-type t1 tenv) ,(final-type t2 tenv))
    [`(,(? type-variable? t1) ,(? type-variable? t2)) 
      (letv* ([(t1 t2) (swap-if-less t1 t2 symbol-less)])
             (values (not (eq? t1 t2)) (ext-table t2 t1 tenv)))]
    [`(,(? type-variable? t1) ,t2) (values #t (ext-table t1 t2 tenv))]
    [`(,t1 ,(? type-variable? t2)) (values #t (ext-table t2 t1 tenv))]
    [`(,(? builtin-type? t1) ,(? builtin-type? t2)) (values (eq? t1 t2) tenv)]
    [`((,t11 -> ,t12) (,t21 -> ,t22)) 
      (letv* ([(v1 tenv) (unify t11 t21 tenv)]
              [(v2 tenv) (unify t12 t22 tenv)])
             (values (and v1 v2) tenv))]
    [_ (values #f tenv)]))

(define (infer v)
  (define (infer-1 v venv tenv)
    (match 
      v
      [(? boolean?) (values 'bool tenv)]
      [(? number?) (values 'int tenv)]
      [(? string?) (values 'string tenv)]
      [(? symbol?) (values (lookup v venv) tenv)]
      [`(if ,test ,conseq ,alt) 
        (letv* ([(testt tenv) (infer-1 test venv tenv)]
                [(u1 tenv) (unify testt 'bool tenv)]
                [(conseqt tenv) (infer-1 conseq venv tenv)]
                [(altt tenv) (infer-1 alt venv tenv)]
                [(u2 tenv) (unify conseqt altt tenv)])
               (cond
                 [(not u1) (values (pretty-print `("infer failed:" ,test"(",(final-type testt tenv)")" "should be bool")) tenv)]
                 [(not u2) (values (pretty-print `("infer failed:" ,conseq"(",(final-type conseqt tenv)")" ,alt"(",(final-type altt tenv)")" "should be same type")) tenv) ]
                 [else (values altt tenv)]))]
      [`(lambda (,formal) ,body) 
        (letv* ([(venv) (ext-table formal (gensym) venv)]
                [(bodyt tenv) (infer-1 body venv tenv)])
               (values `(,(lookup formal venv) -> ,bodyt) tenv))]
      [`(,f ,actual)
        (letv* ([(ft tenv) (infer-1 f venv tenv)]
                [(formalt resultt) (values (gensym) (gensym))]
                [(u1 tenv) (unify ft `(,formalt -> ,resultt) tenv)]
                [(actualt tenv) (infer-1 actual venv tenv)]
                [(u2 tenv) (unify formalt actualt tenv)])
               (cond
                 [(not u1) (values (pretty-print `("infer failed: " ,f"(",(final-type ft tenv)")" "should be a function")) tenv)]
                 [(not u2) (values (pretty-print `("infer failed: " ,f"(",(final-type ft tenv)")" ,actual"(",(final-type actualt tenv)")" "application type mismatch")) tenv)] 
                 [else (values resultt tenv)]))]))
  (letv* ([(t tenv) (infer-1 v builtin-values-type empty)])
         (final-type t tenv)))

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
