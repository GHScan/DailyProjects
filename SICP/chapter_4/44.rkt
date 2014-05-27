#lang racket

(require "35.rkt")

(builtin-register 'abs abs)

(eval G
      '(begin
         (define (a-queens-n n)
           (define (check-board row col board-row board)
             (cond
               [(= 0 board-row) 'ok]
               [(or (= col (car board)) (= (- row board-row) (abs (- col (car board))))) (amb)]
               [else (check-board row col (- board-row 1) (cdr board))])
             )
           (define (a-layout i)
             (if (= 0 i)
               empty
               (let ([small-board (a-layout (- i 1))]
                     [col (an-integer-between 1 n)])
                 (check-board i col (- i 1) small-board)
                 (cons col small-board)))
             )
           (a-layout n)
           )
         (a-queens-n 8)
         ))
