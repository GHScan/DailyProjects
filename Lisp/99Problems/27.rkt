#lang racket

(define (combination m l)
  (let iter ([m m][l l][n (length l)])
    (cond
      [(= m 0) (list empty)]
      [(= m n) (list l)]
      [else 
        (let ([rest1 (map (lambda (sub) (cons (car l) sub)) (iter (- m 1) (cdr l) (- n 1)))]
              [rest2 (iter m (cdr l) (- n 1))])
          (append rest1 rest2))]))
  )

(define (list-remove l1 l2)
  (cond
    [(empty? l1) empty]
    [(member (car l1) l2) (list-remove (cdr l1) l2)]
    [else (cons (car l1) (list-remove (cdr l1) l2))])
  )

(define (group l size-list)
  (if (empty? size-list)
    (list empty)
    (let ([comb (combination (car size-list) l)])
      (apply append
             (map 
               (lambda (first-group) (map (lambda (rest-group) (cons first-group rest-group)) (group (list-remove l first-group) (cdr size-list))))
               comb))))
  )
;------------------------------
(group '(1 2 3) '(1 1 1))
(group '(1 2 3) '(1 2))
(group '(1 2 3) '(2 1))
(group '(1 2 3) '(3))
(group '(1 2 3 4 5 6 7 8 9) '(2 2 5))
