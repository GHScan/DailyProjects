;------------------------------
; quick-sort

(define (quick-sort-classic l)
  (if (or (empty? l) (empty? (cdr l)))
    l
    (let ((first (car l))(rest (cdr l)))
      (append (quick-sort-classic (filter (lambda (i) (< i first)) rest))
              (cons first (quick-sort-classic (filter (lambda (i) (>= i first)) rest))))))
  )

(define (quick-sort-partition l)
  (define (partition l depth v left right)
    (if (empty? l)
      (append (quick-sort-partition left) (cons v (quick-sort-partition right)))
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
  (for-each (lambda (data) (if (equal? (f data) (quick-sort-classic data)) 'ok (error "not ordered!" f))) test-datas))
(for-each test 
          (list 
            quick-sort-classic
            quick-sort-partition
            merge-sort
            bst-sort
            ))
;------------------------------
; benchmark
(define benchmark-datas 
  (list 
    (build-list 500 (lambda (i) (random 500))) ; basic data
    (build-list 1000 (lambda (i) (random 1000))) ; double size
    (build-list 2000 (lambda (i) (random 2000))) 
    (build-list 4000 (lambda (i) (random 4000)))
    (build-list 40000 (lambda (i) (random 40000)))   
    (build-list 1000 (lambda (i) (if (= 0 (remainder i 2)) 500 (random 1000)))) ; half repeat
    (build-list 300 identity) ; ordered
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
            (cons 100000 quick-sort-classic)
            (cons 100000 quick-sort-partition)
            (cons 100000 merge-sort)
            (cons 100000 bst-sort)
            ))
