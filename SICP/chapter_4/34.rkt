#lang racket

(require "25.rkt")
(require "32.rkt")

(eval G 
 '(begin
     (pretty-print (build-list 20 (curry 2 list-ref integers)))
     ))
