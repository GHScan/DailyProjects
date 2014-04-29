#lang racket
(require "69.scm")

(define message '(a b b c c c c d d d d d d d d))
(define tree (generate-huffman-tree (generate-message-pairs message)))
(define encoded-message (encode message tree))
(define decoded-message (decode encoded-message tree))
(pretty-print tree)
(pretty-print encoded-message)
(pretty-print (equal? decoded-message message))
