#lang racket

(provide (all-defined-out))

;------------------------------
(define (combination l m)
  (if (zero? m)
    (list empty)
    (flatten-map
      (lambda (i)
        (let ([v (list-ref l i)][rest (combination (drop l (+ i 1)) (- m 1))])
          (map (lambda (sub) (cons v sub)) rest)))
      (range (length l))))
  )

(define (partial-permuation l m)
  (if (zero? m)
    (list empty)
    (flatten-map 
      (lambda (v)
        (let ([rest (partial-permuation (remove v l) (- m 1))])
          (map (lambda (sub) (cons v sub)) rest)))
      l))
  )

(define (divisible? n m)
  (= 0 (remainder n m))
  )

(define (list->number l)
  (foldl (lambda (v init) (+ v (* 10 init))) 0 l)
  )

(define (number->list n)
  (do ([n n (quotient n 10)][result empty (cons (remainder n 10) result)])
    ((< n 10) (cons n result)))
  )

(define (rotate-left l step)
  (let-values ([(left right) (split-at l (remainder step (length l)))])
    (append right left))
  )

(define (uniq-list l)
  (set->list (list->set l))
  )

(define (list-intersect l1 l2)
  (cond
    [(empty? l1) empty]
    [(member (car l1) l2) (cons (car l1) (list-intersect (cdr l1) l2))]
    [else (list-intersect (cdr l1) l2)])
  )

(define (list-union l1 l2)
  (cond
    [(empty? l1) l2]
    [(not (member (car l1) l2)) (cons (car l1) (list-union (cdr l1) l2))]
    [else (list-union (cdr l1) l2)])
  )

(define (list-subtract l1 l2)
  (cond
    [(empty? l1) empty]
    [(not (member (car l1) l2)) (cons (car l1) (list-subtract (cdr l1) l2))]
    [else (list-subtract (cdr l1) l2)])
  )

(define (palindrome? l)
  (equal? l (reverse l))
  )

(define (group-by l key)
  (define ordered (sort l (lambda (a b) (< (key a) (key b)))))
  (let iter ([group (list (car ordered))][rest (cdr ordered)])
    (cond
      [(empty? rest) (list group)]
      [(= (key (car rest)) (key (car group))) (iter (cons (car rest) group) (cdr rest))]
      [else (cons group (iter (list (car rest)) (cdr rest)))]))
  )
;------------------------------
; stream
(define (stream-merge key . streams)
  (if (empty? (cdr streams))
    (car streams)
    (let iter ([a (car streams)][b (apply stream-merge key (cdr streams))])
      (cond
        [(= (key (stream-first a)) (key (stream-first b))) (stream-cons (stream-first a) (iter (stream-rest a) (stream-rest b)))]
        [(< (key (stream-first a)) (key (stream-first b))) (stream-cons (stream-first a) (iter (stream-rest a) b))]
        [else (stream-cons (stream-first b) (iter a (stream-rest b)))])))
  )

(define (stream-intersect key . streams)
  (if (> (length streams) 2)
    (stream-intersect key (car streams) (apply stream-intersect key (cdr streams)))
    (let iter ([a (car streams)][b (cadr streams)])
      (cond
        [(= (key (stream-first a)) (key (stream-first b))) (stream-cons (stream-first a) (iter (stream-rest a) (stream-rest b)))]
        [(< (key (stream-first a)) (key (stream-first b))) (iter (stream-rest a) b)]
        [else (iter a (stream-rest b))])))
  )

(define (stream-subtract key s1 s2)
  (cond
    [(= (key (stream-first s1)) (key (stream-first s2))) (stream-subtract key (stream-rest s1) (stream-rest s2))]
    [(< (key (stream-first s1)) (key (stream-first s2))) (stream-cons (stream-first s1) (stream-subtract key (stream-rest s1) s2))]
    [else (stream-subtract key s1 (stream-rest s2))])
  )

(define (stream-take s n)
  (if (= 0 n)
    empty-stream
    (stream-cons (stream-first s) (stream-take (stream-rest s) (- n 1))))
  )

(define (stream-take-until s proc)
  (if (proc (stream-first s))
    empty-stream
    (stream-cons (stream-first s) (stream-take-until (stream-rest s) proc)))
  )

(define (stream-map proc . streams)
  (stream-cons (apply proc (map stream-first streams))
               (apply stream-map proc (map stream-rest streams)))
  )

(define (stream-last s)
  (if (stream-empty? (stream-rest s))
    (stream-first s)
    (stream-last (stream-rest s)))
  )

(define (stream-pairs weight s1 s2)
  (let ([first1 (stream-first s1)][rest1 (stream-rest s1)]
        [first2 (stream-first s2)][rest2 (stream-rest s2)])
    (stream-cons (list first1 first2) 
                 (stream-merge 
                   weight
                   (stream-map (lambda (v) (list first1 v)) rest2)
                   (stream-pairs weight rest1 rest2))))
  )

(define (stream-group-by key s)
  (let iter ([group (list (stream-first s))][s (stream-rest s)])
    (if (= (key (car group)) (key (stream-first s)))
      (iter (cons (stream-first s) group) (stream-rest s))
      (stream-cons group (iter (list (stream-first s)) (stream-rest s)))))
  )
;------------------------------
; memoization
(define (memoize f)
  (let ([cache (make-hash)])
    (lambda args
      (let ([v (hash-ref cache args false)])
        (if v
          v
          (begin (set! v (apply f args))
                 (hash-set! cache args v)
                 v)))))
  )

;------------------------------
; primes
(define prime-list (stream-cons 2 (stream-filter prime? (in-naturals 3))))
(define (prime? n)
  (let iter ([prime-list prime-list][max-i (floor (sqrt n))])
    (if (> (stream-first prime-list) max-i)
      true
      (and (not (= 0 (remainder n (stream-first prime-list)))) (iter (stream-rest prime-list) max-i))))
  )

(define (find-prime-factors n)
  (let iter ([prime-list prime-list][n n][result empty])
    (let ([prime (stream-first prime-list)])
      (cond
        [(= 1 n) result]
        [(> (* prime prime) n) (cons n result)]
        [(= 0 (remainder n prime)) (iter prime-list (quotient n prime) (cons prime result))]
        [else (iter (stream-rest prime-list) n result)]))
    )
  )

(define (find-prime-factors-pair n)
  (let iter ([prime-list prime-list][n n][result empty])
    (let ([prime (stream-first prime-list)])
      (cond
        [(= n 1) result]
        [(> (* prime prime) n) (cons (cons n 1) result)]
        [(= 0 (remainder n prime)) 
         (do ([count 1 (+ count 1)][n (quotient n prime) (quotient n prime)])
           ((not (= 0 (remainder n prime))) (iter (stream-rest prime-list) n (cons (cons prime count) result))))]
        [else (iter (stream-rest prime-list) n result)]))
    )
  )

(define (divisors n)
  (let iter ([pairs (find-prime-factors-pair n)])
    (if (empty? pairs)
      (list 1)
      (let ([rest (iter (cdr pairs))])
        (flatten-map
          (lambda (i)
            (let ([multiper (expt (caar pairs) i)])
              (map (lambda (v) (* v multiper)) rest)))
          (range (+ 1 (cdar pairs))))))) 
  )

(define (proper-divisors n)
  (drop-right (divisors n) 1)
  )

(define (make-prime-table n)
  (let ([table (make-vector n true)])
    (vector-set! table 0 false)
    (vector-set! table 1 false)
    (let iter ([i 2])
      (cond
        [(>= i n) table]
        [(not (vector-ref table i)) (iter (+ i 1))]
        [else 
          (do ([j (+ i i) (+ j i)])
            ((>= j n) (iter (+ i 1)))
            (vector-set! table j false))])))
  )
;------------------------------
; list compreshension
(define (flatten-map proc l)
  (apply append (map proc l))
  )

(define-syntax list-comprehension
  (syntax-rules (<-)
    [(_ exp) (list exp)]
    [(_ exp (x <- alist) rules ...) (flatten-map (lambda (x) (list-comprehension exp rules ...)) alist)]
    [(_ exp filter rules ...) (if filter (list-comprehension exp rules ...) empty)]
    )
  )
