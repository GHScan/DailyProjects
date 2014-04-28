#lang racket

(define (variable? e) 
  (symbol? e)
  )
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2))
  )

(define (sum? e)
  (and (pair? e) (eq? '+ (car e)))
  )
(define (addend e)
  (cadr e)
  )
(define (augend e)
  (caddr e)
  )
(define (make-sum a1 a2)
  (cond
    ((equal? a1 0) a2)
    ((equal? a2 0) a1)
    ((and (number? a1) (number? a2)) (+ a1 a2))
    (else (list '+ a1 a2)))
  )

(define (product? e)
  (and (pair? e) (eq? '* (car e)))
  )
(define (multiplier e)
  (cadr e)
  )
(define (multiplicant e)
  (caddr e)
  )
(define (make-product m1 m2)
  (cond
    ((equal? m1 1) m2)
    ((equal? m2 1) m1)
    ((or (equal? m1 0) (equal? m2 0)) 0)
    ((and (number? m1) (number? m2)) (* m1 m2))
    (else (list '* m1 m2)))
  )

(define (exponentiation? e)
  (and (pair? e) (eq? '** (car e)))
  )
(define (base e)
  (cadr e)
  )
(define (exponent e)
  (caddr e)
  )
(define (make-exponentiation b e)
  (cond
    ((equal? e 0) 1)
    ((equal? e 1) b)
    ((equal? b 0) 0)
    ((equal? b 1) 1)
    ((and (number? b) (number? e)) (expt b e))
    (else (list '** b e)))
  )

(define (deriv e var)
  (cond 
    ((number? e) 0)
    ((and (variable? e) (not (same-variable? e var))) 0)
    ((same-variable? e var) 1)
    ((sum? e)
     (make-sum (deriv (addend e) var)
               (deriv (augend e) var)))
    ((product? e)
     (make-sum
       (make-product (multiplier e)
                     (deriv (multiplicant e) var))
       (make-product (deriv (multiplier e) var)
                     (multiplicant e))
       ))
    ((exponentiation? e) 
     (make-product (deriv (base e) var)
                   (make-product (exponent e)
                                 (make-exponentiation (base e) 
                                                      (- (exponent e) 1)))))
    (else (error "invalid expresion!" e))))

(define (infix-to-prefix e)
  (define (assoc-op e)
    (cond 
      ((= 3 (length e)) 
       (list (list-ref e 1) (list-ref e 0) (list-ref e 2)))
      ((and (eq? '+ (list-ref e 1)) (eq? '* (list-ref e 3))) 
       (assoc-op 
         (list* (list-ref e 0) 
                (list-ref e 1) 
                (list (list-ref e 3) (list-ref e 2) (list-ref e 4)) 
                (list-tail e 5))))
      (else 
        (assoc-op 
          (cons 
            (list (list-ref e 1) (list-ref e 0) (list-ref e 2)) 
            (list-tail e 3)))))
    )
  (if (not (pair? e))
    e
    (assoc-op (map infix-to-prefix e)))
  )

(define (deriv-infix e var)
  (deriv (infix-to-prefix e) var)
  )

(pretty-print (deriv-infix '(x + 3) 'x))
(pretty-print (deriv-infix '(x * y) 'x))
(pretty-print (deriv-infix '(x * y * (x + 3)) 'x))
(pretty-print (deriv-infix '((x ** 2) + (x * 2) + 1) 'x))
