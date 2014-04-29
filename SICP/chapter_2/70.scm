#lang racket
(require "69.scm")

(define message '(a a b b a a a a c))
(define tree (generate-huffman-tree (generate-message-pairs message)))
(define encoded-message (encode message tree))
(define decoded-message (decode encoded-message tree))
(pretty-print message)
(pretty-print tree)
(pretty-print encoded-message)
(pretty-print decoded-message)
(pretty-print (equal? message decoded-message))

(define pairs2 '((a 2)(na 16)(boom 1)(sha 3)(get 2)(yip 9)(job 2)(wah 1)))
(define tree2 (generate-huffman-tree pairs2))
(define message2 '(get a job
                   sha na na na na na na na na
                   get a job
                   sha na na na na na na na na
                   wah yip yip yip yip yip yip yip yip yip
                   sha boom))
(define encoded-message2 (encode message2 tree2))
(define decoded-message2 (decode encoded-message2 tree2))
(printf "~a\n" encoded-message2)
(printf "~a\n" decoded-message2)
(pretty-print (equal? message2 decoded-message2))
