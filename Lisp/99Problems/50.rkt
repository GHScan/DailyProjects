#lang racket

(define (insert key v l)
  (cond
    [(empty? l) (list v)]
    [(<= (key v) (key (car l))) (cons v l)]
    [else (cons (car l) (insert key v (cdr l)))])
  )

(define (build-tree freq-sym)
  (let iter ([init-list (foldl (curry insert car) empty freq-sym)])
    (if (empty? (cdr init-list))
      (car init-list)
      (iter (insert car 
                    (list (+ (caar init-list) (caadr init-list)) (car init-list) (cadr init-list))
                    (cddr init-list)))))
  )

(define (encode tree)
  (let traverse ([node tree][code empty])
    (cond
      [(empty? (cddr node)) (list (list (cadr node) (list->string (reverse code))))]
      [else (append
              (traverse (cadr node) (cons #\0 code))
              (traverse (caddr node) (cons #\1 code)))]))
  )

(define (huffman sym-freq)
  (encode
    (build-tree (map (lambda (pair) (list (cadr pair) (car pair))) sym-freq)))
  )
;------------------------------
(huffman '((a 45)(b 13)(c 12)(d 16)(e 9)(f 5)))
