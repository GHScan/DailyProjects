#lang racket

(provide make-table)

(define (make-table less?)
  (define root empty)

  (define (make-node key value left right)
    (cons (cons key value) (cons left right))
    )
  (define (key-node n) (caar n))
  (define (value-node n) (cdar n))
  (define (left-node n) (cadr n))
  (define (right-node n) (cddr n))

  (define (lookup n key)
    (cond
      ((null? n) false)
      ((less? key (key-node n)) (lookup (left-node n) key))
      ((less? (key-node n) key) (lookup (right-node n) key))
      (else (value-node n)))
    )
  (define (insert n key value)
    (cond
      ((null? n) (make-node key value empty empty))
      ((less? key (key-node n)) (make-node (key-node n) (value-node n) 
                                           (insert (left-node n) key value) (right-node n)))
      ((less? (key-node n) key) (make-node (key-node n) (value-node n) 
                                           (left-node n) (insert (right-node n) key value)))
      (else n))
    )
  (lambda (m . args)
    (cond
      ((eq? m 'lookup) (apply lookup root args))
      ((eq? m 'insert!) (set! root (apply insert root args)))
      (else (error "invalid message!")))
    )
  )

;

(define t1 (make-table <))
(t1 'insert! 0 0)
(t1 'insert! 2 2)
(t1 'insert! 3 3)
(t1 'insert! 32 32)
(t1 'lookup 0)
(t1 'lookup 1)
(t1 'lookup 2)
(t1 'lookup 3)
(t1 'lookup 4)
(t1 'lookup 5)
