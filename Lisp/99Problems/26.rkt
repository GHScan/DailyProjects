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
;------------------------------
((lambda (l) (printf "~a: ~a\n" (length l) l)) (combination 1 '(a b c)))
((lambda (l) (printf "~a: ~a\n" (length l) l)) (combination 2 '(a b c)))
((lambda (l) (printf "~a: ~a\n" (length l) l)) (combination 3 '(a b c)))
((lambda (l) (printf "~a: ~a\n" (length l) l)) (combination 3 '(a b c d e f)))
