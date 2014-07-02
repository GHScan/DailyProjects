(define (make-class op)
  (class (x y)
         (define (value) (op x y))
         ))

(define A (make-class +))
(define B (make-class *))
(define A2 
  (class (x y)
         (define (value) (+ x y)))
  )
(define B2 
  (class (x y)
         (define (value) (* x y)))
  )


(define (sqr-value o)
 (pretty-print (* ((method o value)) ((method o value))))
  )

(sqr-value (A 1 2))
(sqr-value (B 3 4))
(sqr-value (A2 1 2))
(sqr-value (B2 3 4))
