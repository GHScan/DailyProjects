#lang racket

(define (my-reverse l)
  (let iter ([l l][result empty])
    (if (empty? l)
      result
      (iter (cdr l) (cons (car l) result))))
  )

(define (my-equal a b)
  (cond
    [(empty? a) (empty? b)]
    [(empty? b) (empty? a)]
    [else (and (equal? (car a) (car b)) (my-equal (cdr a) (cdr b)))])
  )

(define (is-palindrome l)
  (my-equal l (my-reverse l))
  )
;------------------------------
(is-palindrome '(x a m a x))
(is-palindrome '(x a m m a x))
(is-palindrome '(a m a x))
(is-palindrome '(x a m m a x a))
