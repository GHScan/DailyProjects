#lang racket

;------------------------------
(define (match p e sk fk)
  (cond
    [(eq? p '_) (sk empty)]
    [(and (empty? p) (empty? e)) (sk empty)]
    [(and (pair? p) (eq? (car p) 'quote)) (if (equal? (cadr p) e) (sk empty) (fk))]
    [(and (pair? p) (pair? e)) 
     (match (car p) (car e) 
            (lambda (head-match) 
              (if (and (pair? (cdr p)) (eq? (cadr p) '...))
                (let ([head-match (map (lambda (kv) (cons (car kv) (list (cdr kv)))) head-match)])
                  (sk (foldl (lambda (e head-match) 
                               (match (car p) e 
                                      (lambda (e-match) 
                                        (map (lambda (kv-list kv) 
                                               (cons (car kv-list) (append (cdr kv-list) (list (cdr kv))))) 
                                             head-match e-match)) 
                                      fk)) 
                             head-match (cdr e))))
                (match (cdr p) (cdr e) 
                       (lambda (tail-match) (sk (append head-match tail-match))) fk))) 
            fk)]
    [(and (symbol? p)) (sk (list (cons p e)))]
    [(and (procedure? p) (p e)) (sk empty)]
    [else (fk)])
  )

;------------------------------
(define (syntax-case e p)
  (match p e identity (lambda () false))
  )
(define (printer name)
  (lambda (m) (printf " ~a => ~a\n" name m))
  )

(define (syntax-dispatch e)
  (cond
    [(syntax-case e number?) => (printer "number")]
    [(syntax-case e string?) => (printer "string")]
    [(syntax-case e symbol?) => (printer "symbol")]
    [(syntax-case e '('quote e)) => (printer "quote")]
    [(syntax-case e '('if pred then else)) => (printer "if")]
    [(syntax-case e '('begin . exp-list)) => (printer "begin")]
    [(syntax-case e '('lambda args . exp-list)) => (printer "lambda")]
    [(syntax-case e '('define (var . args) . exp-list)) => (printer "define lambda")]
    [(syntax-case e '('define var exp)) => (printer "define varaible")]
    [(syntax-case e '('set! var exp)) => (printer "set!")]
    [(syntax-case e '('cond (case-pred . case-exp-list) . rest-case)) => (printer "cond")]
    [(syntax-case e '('and exp . rest-exp)) => (printer "and")]
    [(syntax-case e '('or exp . rest-exp)) => (printer "or")]
    [(syntax-case e '('let ((var-list value-list) ...) . exp-list)) => (printer "let")]
    [(syntax-case e '('let name ((var-list value-list) ...) . exp-list)) => (printer "named let")]
    [(syntax-case e '('let* ((var-list value-list) ...) . exp-list)) => (printer "let*")]
    [(syntax-case e '('letrec ((var-list value-list) ...) . exp-list)) => (printer "letrec")]
    [(syntax-case e '(p . args)) => (printer "application")])
)
;------------------------------

(for-each syntax-dispatch
 '(1
     "1"
     a
     '(1 2 3)
     (if (= 0 n) a (fib b (+ a b) (- n 1)))
     (begin 1 2 (print x) x)
     (lambda (a b) (define c (+ a b)) (+ c c))
     (define a 1)
     (define (add a b) (+ a b))
     (set! a (+ a 1))
     (cond [(= n 0) a][(= n 1) b][else (+ a b)])
     (and (pair? p) (eq? (car p) 'quote))
     (or (empty? p) (eq? (car p) 'eof))
     (let ([x 1][y 2]) (define z (+ x y)) (+ x y))
     (let name ([x 1][y 2]) (define z (+ x y)) (+ x y))
     (let* ([x 1][y 2]) (define z (+ x y)) (+ x y))
     (letrec ([x 1][y 2]) (define z (+ x y)) (+ x y))
     (+ 2 1)
     ((lambda (a b) (+ a b)) 2 1)
     )
 )
