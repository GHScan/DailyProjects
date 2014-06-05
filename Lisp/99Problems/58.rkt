#lang racket

(define (flatten-map proc l)
  (apply append (map proc l))
  )

(define (cbal-tree n)
  (cond [(zero? n) (list empty)]
        [(= 1 n) (list (list 'x empty empty))]
        [else 
          (let* ([n1 (quotient (- n 1) 2)]
                 [n2 (- n 1 n1)]
                 [c1-list (cbal-tree n1)]
                 [c2-list (if (= n1 n2) c1-list (cbal-tree n2))])
            (flatten-map 
              (lambda (c1)
                (flatten-map
                  (lambda (c2)
                    (if (= n1 n2)
                      (list (list 'x c1 c2))
                      (list (list 'x c1 c2) (list 'x c2 c1))))
                  c2-list))
              c1-list))])
  )

(define (mirror? a b)
  (cond
    [(empty? a) (empty? b)]
    [(empty? b) (empty? a)]
    [else (and (mirror? (cadr a) (caddr b)) (mirror? (caddr a) (cadr b)))])
  )

(define (symmetirc? a)
  (cond
    [(empty? a) false]
    [else (mirror? (cadr a) (caddr a))])
  )

(define (sym-cbal-trees n)
 (filter symmetirc? (cbal-tree n))
 )
;------------------------------
(length (sym-cbal-trees 5))
(length (sym-cbal-trees 57))
