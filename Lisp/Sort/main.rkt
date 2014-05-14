#lang racket

;------------------------------
; insertion-sort
(define (insert v l)
  (if (empty? l)
    (list v)
    (let ((first (car l))(rest (cdr l)))
      (cond
        ((<= v first) (cons v l))
        ((> v first) (cons first (insert v rest))))))
  )

(define (insertion-sort l)
  (if (empty? l)
    empty
    (insert (car l) (insertion-sort (cdr l))))
  )

;------------------------------
; bubble-sort
(define (bubble l)
  (cond 
    ((empty? l) empty)
    ((empty? (cdr l)) l)
    (else 
      (let ((first (car l))(rest (bubble (cdr l))))
        (if (<= first (car rest))
          (cons first rest)
          (cons (car rest) (cons first (cdr rest)))))))
  )

(define (bubble-sort l)
  (if (empty? l)
    empty
    (let ((newl (bubble l)))
      (cons (car newl) (bubble-sort (cdr newl)))))
  )

;------------------------------
; quick-sort

; curry is too slow, because it should concat the args every time
(define (quick-sort l)
  (if (or (empty? l) (empty? (cdr l)))
    l
    (let ((first (car l))(rest (cdr l)))
      (append (quick-sort (filter (curry > first) rest))
              (cons first (quick-sort (filter (curry <= first) rest))))))
  )

(define (quick-sort-v2 l)
  (if (or (empty? l) (empty? (cdr l)))
    l
    (let ((first (car l))(rest (cdr l)))
      (append (quick-sort-v2 (filter (lambda (i) (< i first)) rest))
              (cons first (quick-sort-v2 (filter (lambda (i) (>= i first)) rest))))))
  )

(define (quick-sort-v3 l)
  (define (partition l depth v left right)
    (if (empty? l)
      (append (quick-sort-v3 left) (cons v (quick-sort-v3 right)))
      (if (or (< (car l) v) (and (= (car l) v) (= 0 (remainder depth 2))))
        (partition (cdr l) (add1 depth) v (cons (car l) left) right)
        (partition (cdr l) (add1 depth) v left (cons (car l) right)))
      ))
  (if (empty? l)
    l
    (partition (cdr l) 0 (car l) empty empty))
  )

;------------------------------
; merge-sort

(define (merge l r)
  (cond
    ((empty? l) r)
    ((empty? r) l)
    (else 
      (let ((lfirst (car l))(lrest (cdr l))(rfirst (car r))(rrest (cdr r)))
        (cond
          ((< lfirst rfirst) (cons lfirst (merge lrest r)))
          ((> lfirst rfirst) (cons rfirst (merge l rrest)))
          (else (cons lfirst (cons rfirst (merge lrest rrest))))))))
  )

(define (merge-sort l)
  (define (prefix-merge-sort l n)
    (cond
      ((= n 0) (cons empty l))
      ((= n 1) (cons (list (car l)) (cdr l)))
      (else 
        (let ((half (quotient n 2)))
          (let ((left (prefix-merge-sort l half)))
            (let ((right (prefix-merge-sort (cdr left) (- n half))))
              (cons (merge (car left) (car right)) (cdr right)))))))
    )
  (car (prefix-merge-sort l (length l)))
  )

;------------------------------
; bst-sort
(define (bst-insert v node)
  (if (empty? node)
    (list v empty empty)
    (let ((nv (car node))(left (cadr node))(right (caddr node)))
      (if (<= v nv)
        (list nv (bst-insert v left) right)
        (list nv left (bst-insert v right)))))
  )

(define (bst-infix-traverse node)
  (if (empty? node)
    empty
    (let ((nv (car node))(left (cadr node))(right (caddr node)))
      (append (bst-infix-traverse left) 
              (cons nv (bst-infix-traverse right)))))
  )

(define (bst-sort l)
  (bst-infix-traverse (foldl bst-insert empty l))
  )

;------------------------------
; test
(define test-datas (map 
                     (lambda (len) (build-list len (lambda (i) (random len))))
                     (list 1 3 4 5 7 8 9 10 11 15 16 32 33 64 128 254 255 256 257)))
(define (test f)
  (for-each (lambda (data) (if (equal? (f data) (sort data <)) 'ok (error "not ordered!" f))) test-datas))
(for-each test 
          (list 
            insertion-sort
            bubble-sort
            quick-sort
            quick-sort-v2
            quick-sort-v3
            merge-sort
            bst-sort
            (lambda (data) (sort data <))
            ))
;------------------------------
; benchmark
(define benchmark-datas 
  (list 
    (build-list 5000 (lambda (i) (random 5000))) ; basic data
    (build-list 10000 (lambda (i) (random 10000))) ; double size
    (build-list 20000 (lambda (i) (random 20000))) 
    (build-list 40000 (lambda (i) (random 40000)))
    (build-list 400000 (lambda (i) (random 400000)))   
    (build-list 1000000 (lambda (i) (random 1000000)))  ; huge randoms
    (build-list 10000 (lambda (i) (if (= 0 (remainder i 2)) 5000 (random 10000)))) ; half repeat
    (build-list 3000 identity) ; ordered
    ))

(define (benchmark max-len f)
  (printf "~a:\n" f)
  (for-each (lambda (data) 
              (let ((start-time (current-inexact-milliseconds)))
                (f data)
                (printf "\t ~a -> ~a\n" (length data) (- (current-inexact-milliseconds) start-time))))
            (filter (lambda (data) (<= (length data) max-len)) benchmark-datas))
  )

(for-each (lambda (pair) (benchmark (car pair) (cdr pair)))
          (list 
            (cons 20000 insertion-sort)
            (cons 20000 bubble-sort)
            (cons 500000 quick-sort)
            (cons 1000000 quick-sort-v2)
            (cons 1000000 quick-sort-v3)
            (cons 1000000 merge-sort)
            (cons 1000000 bst-sort)
            (cons 1000000 (lambda (data) (sort data <)))
            ))
