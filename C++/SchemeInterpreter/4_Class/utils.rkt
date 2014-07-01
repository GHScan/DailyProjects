#lang racket

(provide (all-defined-out))
;------------------------------
(define (build-label-map codes k)
  (cond
    [(empty? codes) (k empty empty)]
    [(eq? 'label (caar codes)) 
     (build-label-map (cdr codes) 
                      (lambda (labels new-codes)
                        (k (cons (cons (cadar codes) new-codes) labels) new-codes)))]
    [else 
      (build-label-map (cdr codes)
                       (lambda (labels new-codes)
                         (k labels (cons (car codes) new-codes))))])
  )

