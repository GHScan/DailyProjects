#lang racket
(require racket/pretty)

(define l1 (list 1 3 (list 5 7) 9))
(pretty-print l1)
(pretty-print (cadr (caddr l1)))
    
(define l2 (list (list 7)))
(pretty-print l2)
(pretty-print (caar l2))

(define l3 (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))
(pretty-print l3)
(pretty-print (cadadr (cadadr (cadadr l3))))
