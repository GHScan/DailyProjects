(define factorial (lambda (n r) (if (= 1 n) r (factorial (- n 1) (* r n)))))
(define range
   (lambda (first limit)
     (if (= first limit) empty (cons first (range (+ first 1) limit)))))
(define range-cps
   (lambda (first limit k)
     (if (= first limit)
       (k empty)
       (range-cps (+ first 1) limit (lambda (rest) (k (cons first rest)))))))
(define map
   (lambda (f l) (if (empty? l) empty (cons (f (car l)) (map f (cdr l))))))
(define map-cps
   (lambda (f l k)
     (if (empty? l)
       (k empty)
       (map-cps f (cdr l) (lambda (rest) (k (cons (f (car l)) rest)))))))
(define build-list (lambda (n f) (map f (range 0 n))))
(define build-list-cps
   (lambda (n f k) (range-cps 0 n (lambda (rest) (map-cps f rest k)))))
(define filter
   (lambda (f l)
     (if (empty? l)
       (begin l)
       (if (f (car l))
         (begin (cons (car l) (filter f (cdr l))))
         (begin (filter f (cdr l)))))))
(define filter-cps
   (lambda (f l k)
     (if (empty? l)
       (begin (k empty))
       (if (f (car l))
         (begin (filter-cps f (cdr l) (lambda (rest) (k (cons (car l) rest)))))
         (begin (filter-cps f (cdr l) k))))))
(define even? (lambda (n) (= 0 (remainder n 2))))
(define odd? (lambda (n) (not (even? n))))
(define fib (lambda (n) (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2))))))
(define fib-dp (lambda (n a b) (if (= n 0) a (fib-dp (- n 1) b (+ a b)))))
(define sort
   (lambda (l cmp)
     (if (empty? l)
       empty
       (append
        (sort (filter (lambda (v) (cmp v (car l))) (cdr l)) cmp)
        (cons
         (car l)
         (sort (filter (lambda (v) (not (cmp v (car l)))) (cdr l)) cmp))))))
(define sort-cps
   (lambda (l cmp k)
     (if (empty? l)
       (k empty)
       (filter-cps
        (lambda (v) (cmp v (car l)))
        (cdr l)
        (lambda (left)
          (sort-cps
           left
           cmp
           (lambda (left)
             (filter-cps
              (lambda (v) (not (cmp v (car l))))
              (cdr l)
              (lambda (right)
                (sort-cps
                 right
                 cmp
                 (lambda (right)
                   (k (append left (cons (car l) right))))))))))))))
(define time-it
   (lambda (f)
     (begin
       (define start (current-inexact-milliseconds))
       (f)
       (pretty-print (- (current-inexact-milliseconds) start)))))
(time-it (lambda () (fib 15)))
(time-it (lambda () (fib-dp 15 0 1)))
(time-it (lambda () (filter even? (map sqr (range 0 100)))))
(time-it (lambda () (factorial 100 1)))
(time-it (lambda () (eval '(factorial 100 1))))
(define random-list (build-list 200 (lambda (i) (random 1000))))
(time-it (lambda () (sort random-list <)))
(time-it (lambda () (sort-cps random-list < identity)))
