#lang racket

(require "utils.rkt")

;(build-list 30 (curry stream-ref prime-list))

(stream-ref prime-list 10000)
