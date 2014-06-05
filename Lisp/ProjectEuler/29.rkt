#lang racket

(define nums (list->set empty))

(do ([a 2 (+ 1 a)])
  ((> a 100))
  (do ([b 2 (+ 1 b)])
    ((> b 100))
    (set! nums (set-add nums (expt a b)))))

(length (set->list nums))
