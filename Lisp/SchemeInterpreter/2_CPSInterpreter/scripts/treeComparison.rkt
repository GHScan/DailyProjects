;------------------------------
; tree -> list
(define (tree-equal-list? t1 t2) 
  (define (generate-leaves t)
    (cond
      [(empty? t) empty]
      [(pair? t) (append (generate-leaves (car t)) (generate-leaves (cdr t)))]
      [else (list t)]
      )
    )
  (equal? (generate-leaves t1) (generate-leaves t2))
  )

;------------------------------
; yield (call/cc)
(define (tree-equal-yield? t1 t2)
  (define (generate-leaves t)
    (letrec ([main-k false]
             [iter-k false]
             [yield 
               (lambda (value)
                 (call/cc (lambda (k)
                            (set! iter-k k)
                            (main-k value))))]
             [traverse 
               (lambda (t)
                 (cond 
                   [(empty? t) 'ok]
                   [(pair? t) (traverse (car t)) (traverse (cdr t))]
                   [else (yield t)]))])
      (lambda ()
        (call/cc 
          (lambda (k)
            (set! main-k k)
            (if iter-k 
              (iter-k)
              (begin (traverse t) (yield empty)))))))
    )
  (let ([iter1 (generate-leaves t1)][iter2 (generate-leaves t2)])
    (do ([v1 (iter1) (iter1)][v2 (iter2) (iter2)])
      ((or (not (equal? v1 v2)) (eq? v1 empty)) (and (eq? v1 empty) (eq? v2 empty))))) 
  )

;------------------------------
; variant cps
(define (tree-equal-vcps? t1 t2)
  (define (generate-leaves t)
    (define (traverse t continue)
      (cond
        [(empty? t) (continue empty)]
        [(pair? t) (traverse (car t) 
                             (lambda (_)
                               (traverse (cdr t) continue)))]
        [else (cons t continue)])
      )
    (let ([vc (traverse t (lambda (_) (cons empty empty)))])
      (lambda ()
        (let ([value (car vc)])
          (if (empty? value)
            value
            (begin (set! vc ((cdr vc) (car vc))) value)))))
    )
  (let ([iter1 (generate-leaves t1)][iter2 (generate-leaves t2)])
    (do ([v1 (iter1) (iter1)][v2 (iter2) (iter2)])
      ((or (not (equal? v1 v2)) (eq? v1 empty)) (and (eq? v1 empty) (eq? v2 empty)))))
  )

;------------------------------
; test and benchmark
(define (assert b)
  (if b
    'ok
    (error "assert failed"))
  )

(define (construct-random-tree input)
  (let construct ([input input][n (length input)])
    (cond 
      [(zero? n) (cons empty input)]
      [(= n 1) (cons (car input) (cdr input))]
      [else (let* ([right-n (random n)]
                   [left-n (- n right-n)]
                   [left (construct input left-n)]
                   [right (construct (cdr left) right-n)])
              (cons (cons (car left) (car right)) (cdr right)))]))
  )

(define cmps (list 
               tree-equal-list? 
               tree-equal-yield?
               tree-equal-vcps?
               ))

(define (run-test)
  (let ([test-datas (list
                      (cons '(1 (2 3)) '((1 2) 3))
                      (cons '(a (b c) d) '(a b (d e)))
                      (cons '(1 (2 (3 4))) '((((1) 2) 3) 4))
                      (cons (construct-random-tree (range 5)) (construct-random-tree (range 5)))
                      (cons (construct-random-tree (range 5)) (construct-random-tree (range 5)))
                      (cons (construct-random-tree (range 5)) (construct-random-tree (range 5)))
                      ((lambda (x) (cons x x)) (construct-random-tree (range 5)))
                      ((lambda (x) (cons x x)) (construct-random-tree (range 5)))
                      )])
    (do ([rest (cdr cmps) (cdr rest)])
      ((empty? rest))
      (do ([data test-datas (cdr data)])
        ((empty? data))
        (let ([cmp (car rest)][data1 (caar data)][data2 (cdar data)])
          (assert (eq? (cmp data1 data2) ((car cmps) data1 data2)))))))
  )

(define (run-benchmark)
  (let ([datas (list
                 (cons '100-same ((lambda (x) (list x x)) (construct-random-tree (range 100))))
                 (cons '1000-same ((lambda (x) (list x x)) (construct-random-tree (range 1000))))
                 (cons '100-diff-first ((lambda (x) (list (cons 1 x) (cons 2 x))) (construct-random-tree (range 100))))
                 (cons '1000-diff-first ((lambda (x) (list (cons 1 x) (cons 2 x))) (construct-random-tree (range 1000))))
                 (list '100-random (construct-random-tree (range 100)) (construct-random-tree (range 100)))
                 (list '1000-random (construct-random-tree (range 1000)) (construct-random-tree (range 1000)))
                 )])
    (do ([rest cmps (cdr rest)])
      ((empty? rest))
      (printf "~a:\n" (car rest))
      (do ([data datas (cdr data)])
        ((empty? data))
        (let ([cmp (car rest)]
              [dname (caar data)]
              [data1 (cadar data)][data2 (caddar data)]
              [start (current-inexact-milliseconds)])
          (do ([i 0 (+ i 1)])
            ((> i 100))
            (cmp data1 data2))
          (printf "\t~a =>  ~a\n" dname (- (current-inexact-milliseconds) start))
          (assert (eq? (cmp data1 data2) ((car cmps) data1 data2)))))))
  )

(run-test)
(run-benchmark)
