#lang racket

(require "25.rkt")

(eval G
 '(pretty-print (((lambda (a b) (if false a b)) + -) 1 2)))
