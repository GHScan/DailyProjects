#lang racket
(require racket/pretty)

(define (subsets s) 
  (cond
    ((null? s) 
     (list (list)))
    (else  
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (set) (cons (car s) set)) rest)))))
  )

(pretty-print (subsets (list 1 2 3)))
