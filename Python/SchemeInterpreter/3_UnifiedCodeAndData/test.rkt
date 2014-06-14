(define (factorial n r)
  (if (= 1 n)
    r
    (factorial (- n 1) (* r n)))
  )

(define (range first limit)
  (if (= first limit)
    empty
    (cons first (range (+ first 1) limit)))
  )

(define (range-cps first limit k)
  (if (= first limit)
    (k empty)
    (range-cps (+ first 1) limit (lambda (rest) (k (cons first rest)))))
  )

(define (map f l)
  (if (empty? l)
    empty
    (cons (f (car l)) (map f (cdr l))))
  )

(define (map-cps f l k)
  (if (empty? l)
    (k empty)
    (map-cps f (cdr l)
             (lambda (rest) (k (cons (f (car l)) rest)))))
  )

(define (build-list n f)
  (map f (range 0 n))
  )

(define (build-list-cps n f k)
  (range-cps 0 n (lambda (rest) (map-cps f rest k)))
  )

(define (filter f l)
  (cond
    [(empty? l) l]
    [(f (car l)) (cons (car l) (filter f (cdr l)))]
    [else (filter f (cdr l))])
  )

(define (filter-cps f l k)
  (cond
    [(empty? l) (k empty)]
    [(f (car l)) (filter-cps f (cdr l) (lambda (rest) (k (cons (car l) rest))))]
    [else (filter-cps f (cdr l) k)])
  )

(define (even? n)
  (= 0 (remainder n 2))
  )

(define (odd? n)
  (not (even? n))
  )

(define (fib n)
  (if (< n 2)
    n
    (+ (fib (- n 1)) (fib (- n 2))))
  )

(define (fib-dp n a b)
  (if (= n 0)
    a
    (fib-dp (- n 1) b (+ a b)))
  )

(define (sort l cmp)
  (if (empty? l)
    empty
    (append (sort (filter (lambda (v) (cmp v (car l))) (cdr l)) cmp)
            (cons (car l) (sort (filter (lambda (v) (not (cmp v (car l)))) (cdr l)) cmp))))
  )

(define (sort-cps l cmp k)
  (if (empty? l)
    (k empty)
    (filter-cps (lambda (v) (cmp v (car l))) (cdr l) 
                (lambda (left)
                  (sort-cps left cmp 
                            (lambda (left)
                              (filter-cps (lambda (v) (not (cmp v (car l)))) (cdr l)
                                          (lambda (right)
                                            (sort-cps right cmp
                                                      (lambda (right)
                                                        (k (append left (cons (car l) right))))))))))))
  )

(define (time-it f)
  (define start (current-inexact-milliseconds))
  (f)
  (pretty-print (- (current-inexact-milliseconds) start))
  )

;------------------------------
(time-it (lambda () (fib 15)))
(time-it (lambda () (fib-dp 15 0 1)))
(time-it (lambda () (filter even? (map sqr (range 0 100)))))
(time-it (lambda () (factorial 100 1)))
(time-it (lambda () (eval (quote (factorial 100 1)))))

(define random-list (build-list 200 (lambda (i) (random 1000))))
(time-it (lambda () (sort random-list <)))
(time-it (lambda () (sort-cps random-list < identity)))
