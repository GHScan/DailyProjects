#lang racket

(require "main.rkt")

(cps 'x)
(cps '(lambda (x) x))
(cps '(lambda (x) (x 1)))
(cps '(if (f x) a b))
(cps '(if x (f a) b))
(cps '(lambda (x) (if (f x) a b)))
(cps '(lambda (x) (if (if x (f a) b) c d)))
(cps '(lambda (x) (if (if x (zero? a) b) c d)))
(cps '(lambda (x) (if t (if x (f a) b) c)))
(cps '(lambda (x) (if (if t (if x (f a) b) c) e w)))
(cps '(lambda (x) (h (if x (f a) b))))
(cps '(lambda (x) ((if x (f g) h) c)))
(cps '(((f a) (g b)) ((f c) (g d))))


(define fact-cps
  (cps
    '(lambda (n)
       ((lambda (fact)
          ((fact fact) n))
        (lambda (fact)
          (lambda (n)
            (if (zero? n)
              1
              (* n ((fact fact) (sub1 n))))))))))
;; print out CPSed function
(pretty-print fact-cps)
;;; =>
;;; '(lambda (n k)
;;;    ((lambda (fact k) (fact fact (lambda (v0) (v0 n k))))
;;;     (lambda (fact k)
;;;       (k
;;;        (lambda (n k)
;;;          (if (zero? n)
;;;            (k 1)
;;;            (fact
;;;             fact
;;;             (lambda (v1) (v1 (sub1 n) (lambda (v2) (k (* n v2))))))))))
;;;     k))
;
;
(define-namespace-anchor anchor)
((eval fact-cps (namespace-anchor->namespace anchor)) 5 (lambda (v) v))
;; => 120
