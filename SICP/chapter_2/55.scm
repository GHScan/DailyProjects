#lang racket

(pretty-print (car ''abcd))
(pretty-print (car '(quote abcd)))
(pretty-print (car (quote (quote abc))))
