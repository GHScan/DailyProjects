(define (make-class op)
  (class (x y)
         (define (value) (op x y))
         ))

(define A (make-class +))
(define B (make-class *))

(define (sqr-value o)
 (pretty-print (+ ((method o value)) ((method o value))))
  )

(sqr-value (A 1 2))
(sqr-value (B 3 4))
