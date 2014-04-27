#lang racket
(require racket/pretty)

(define (flatmap f l)
  (foldr append (list) (map f l))
  )

(define (any l)
  (foldl (lambda (init x) (or init x)) false l)
  )

(define (queens board-size)
  (define (safe? board row col)
    (not (any (map (lambda (pos)
                     (let ((r (car pos))(c (cdr pos)))
                       (or (= row r) (= (abs (- row r)) (abs (- col c))))))
                   board)))
    )
  (define (queen-cols k)
    (cond
      ((= 0 k) (list (list)))
      (else 
        (flatmap
          (lambda (board)
            (map
              (lambda (row) (cons (cons row k) board))
              (filter 
                (lambda (row)
                  (safe? board row k))
                (build-list board-size add1))))
          (queen-cols (sub1 k))))
      )
    )
  (queen-cols board-size)
  )

(for-each (lambda (board) (display board)(newline)) (queens 8))
