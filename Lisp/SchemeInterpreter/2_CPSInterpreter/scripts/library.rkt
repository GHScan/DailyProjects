; This file can only use fundamental form

(define (build-list n proc)
  (define (iter i result)
    (if (= 0 i)
      result
      (iter (- i 1) (cons (proc (- i 1)) result)))
    )
  (iter n empty)
  )
(define (foldl proc init l)
  (if (null? l)
    init
    (foldl proc (proc (car l) init) (cdr l)))
  )
(define (map proc l)
  (if (null? l)
    empty
    (cons (proc (car l)) (map proc (cdr l))))
  )
(define (filter pred l)
  (if (empty? l)
    empty
    (if (pred (car l))
      (cons (car l) (filter pred (cdr l)))
      (filter pred (cdr l))))
  )
(define (curry n f . boundArgs)
  (lambda args
    ((lambda (all-args)
       (if (>= (length all-args) n)
         (apply f all-args)
         (apply curry n f all-args))) 
     (append boundArgs args)))
  )
(define (for-each proc l)
  (if (empty? l)
    'ok
    (begin (proc (car l))
           (for-each proc (cdr l))))
  )
