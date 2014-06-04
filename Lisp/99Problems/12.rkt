#lang racket

(define (encode-modified l)
  (define (pack-unit v count)
    (if (= 1 count) v (list v count))
    )
  (if (empty? l)
    empty
    (let iter ([v (car l)][count 1][l (cdr l)])
      (cond
        [(empty? l) (list (pack-unit v count))]
        [(not (equal? v (car l))) (cons (pack-unit v count) (iter (car l) 1 (cdr l)))]
        [else (iter v (+ count 1) (cdr l))])))
  )

(define (decode-modified l)
  (define (repeat v count)
    (if (= 0 count) 
      empty 
      (cons v (repeat v (- count 1))))
    )
  (cond
    [(empty? l) empty]
    [(pair? (car l)) (append (repeat (caar l) (cadar l)) (decode-modified (cdr l)))]
    [else (cons (car l) (decode-modified (cdr l)))])
  )
;------------------------------
(decode-modified (encode-modified '()))
(decode-modified (encode-modified '(a)))
(decode-modified (encode-modified '(a b)))
(decode-modified (encode-modified '(a a b)))
(decode-modified (encode-modified '(a b b)))
(decode-modified (encode-modified '(a a b b)))
(decode-modified (encode-modified '(a a a a b c c a a d e e e e)))
