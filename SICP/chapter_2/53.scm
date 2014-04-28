#lang racket

(pretty-print (list 'a 'b 'c))
(pretty-print (list (list 'george)))
(pretty-print (cdr '((x1 x2) (y1 y2))))
(pretty-print (cadr '((x1 x2) (y1 y2))))
(pretty-print (pair? (car '(a short list))))
(pretty-print (memq 'red '((red shoes) (blue socks))))
(pretty-print (memq 'red '(red shoes blue socks)))
