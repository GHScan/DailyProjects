#lang racket

(define (my-flatten l)
  (cond
    [(empty? l) l]
    [(pair? l) (append (my-flatten (car l)) (my-flatten (cdr l)))]
    [else (list l)])
  )
;------------------------------
(my-flatten '(1))
(my-flatten '(1 . 2))
(my-flatten '(1 . (2 . 3)))
(my-flatten '(1 . (2 . (3 . 4))))
(my-flatten '((0 . 1) . (2 . (3 . 4))))
(my-flatten '((0 . 1) . (2 . (3 . (4 5 6)))))
(my-flatten '((0 . (1 1.5)) . (((2) (2.2 . 2.3)) . (3 . (4 5 6)))))
