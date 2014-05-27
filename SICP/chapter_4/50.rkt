#lang racket

(require "35.rkt")

(define (random-choice l)
  (let choose ([l (cdr l)][item (car l)][i 2])
    (if (empty? l)
      item
      (choose (cdr l) (if (= 0 (random i)) (car l) item) (+ i 1))))
  )

(define (random-shuffle l)
  (let iter ([rest l][result empty])
    (if (empty? rest)
      result
      (let ([choose (random-choice rest)])
        (iter (remove choose rest) (cons choose result)))))
  )


(define (compile-ramb exp)
  (let ([cexps (map compile (cdr exp))])
    (lambda (env succ fail)
      ((let register-fail ([cexps (random-shuffle cexps)])
         (if (empty? cexps)
           fail
           (let ([rest-fail (register-fail (cdr cexps))])
             (lambda ()
               ((car cexps) env succ rest-fail))))))))
  )

(special-form-register 'ramb compile-ramb)

(eval G
      '(begin
         (ramb 1 2 3 4)
         ))
