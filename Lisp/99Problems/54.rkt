#lang racket

(define (istree n)
  (if (empty? n)
    true
    (and (= 3 (length n)) (istree (cadr n)) (istree (caddr n))))
  )
;------------------------------
(istree '(a (b () ()) ()))
(istree '(a (b () ())))
