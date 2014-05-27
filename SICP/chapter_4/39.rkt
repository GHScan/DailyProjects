#lang racket

(require "35.rkt")

(builtin-register 'member member)
(builtin-register 'abs abs)

(eval G
      '(begin
         (define (distinc first rest)
           (if (empty? rest)
             true
             (and (not (member first rest)) (distinc (car rest) (cdr rest))))
           )
         (define (mutiple-dwelling)
           (let ([baker (amb 1 2 3 4 5)])
             (assert (not (= baker 5)))
             (let ([cooper (amb 1 2 3 4 5)])
               (assert (not (= cooper 1)))
               (let ([fletcher (amb 1 2 3 4 5)])
                 (assert (and (not (= fletcher 1)) (not (= fletcher 5))))
                 (assert (not (= 1 (abs (- fletcher cooper)))))
                 (let ([miller (amb 1 2 3 4 5)])
                   (assert (> miller cooper))
                   (let ([smith (amb 1 2 3 4 5)])
                     (assert (not (= 1 (abs (- fletcher smith)))))
                     (assert (distinc baker (list cooper fletcher miller smith)))
                     (list baker cooper fletcher miller smith))))))
           )
         (mutiple-dwelling)
         ))
