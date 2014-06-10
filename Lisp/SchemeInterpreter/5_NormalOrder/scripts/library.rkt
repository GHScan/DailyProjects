(set-argument-passing-mechanism 'call-by-need)
;------------------------------
(define the-pair-tag (raw-cons 'pair empty))
(define (cons a b)
  (raw-cons the-pair-tag (lambda (v) (if v a b)))
  )
(define (car p)
  ((raw-cdr p) true)
  )
(define (cdr p)
  ((raw-cdr p) false)
  )
(define (pair? p)
  (and (raw-pair? p) (eq? (raw-car p) the-pair-tag))
  )
;------------------------------
(define (length l)
  (if (empty? l)
    0
    (+ 1 (length (cdr l))))
  )
(define (append a b)
  (if (empty? a)
    b
    (cons (car a) (append (cdr a) b)))
  )
(define (list-ref l i)
  (if (= 0 i)
    (car l)
    (list-ref (cdr l) (- i 1)))
  )
(define (take l n)
  (if (= 0 n)
    empty
    (cons (car l) (take (cdr l) (- n 1))))
  )
;------------------------------
(define (pair->raw-pair p)
  (if (pair? p)
    (raw-cons (pair->raw-pair (car p)) (pair->raw-pair (cdr p)))
    p)
  )
;------------------------------
(define (print v)
  (raw-print (pair->raw-pair v))
  )
(define (pretty-print v)
  (print v)
  (newline)
  )
;------------------------------
(define (foldl proc init l)
  (if (empty? l)
    init
    (foldl proc (proc (car l) init) (cdr l)))
  )
(define (map proc l)
  (if (empty? l)
    empty
    (cons (proc (car l)) (map proc (cdr l))))
  )
(define (map-2 proc l1 l2)
  (if (or (empty? l1) (empty? l2))
    empty
    (cons (proc (car l1) (car l2)) (map-2 proc (cdr l1) (cdr l2))))
  )
(define (filter pred l)
  (cond
    [(empty? l) empty]
    [(pred (car l)) (cons (car l) (filter pred (cdr l)))]
    [else (filter pred (cdr l))])
  )
;------------------------------
(define (range first end)
  (if (= first end)
    empty
    (cons first (range (+ first 1) end)))
  )
(define (build-list n proc)
  (map proc (range 0 n))
  )
(define (for-each proc l)
  (if (empty? l)
    (void)
    (begin (proc (car l))
           (for-each proc (cdr l))))
  )
;------------------------------
