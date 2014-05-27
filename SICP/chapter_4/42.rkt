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
           (let ([betty (amb 1 2 3 4 5)]
                 [ethel (amb 1 2 3 4 5)]
                 [john (amb 1 2 3 4 5)]
                 [katty (amb 1 2 3 4 5)]
                 [mary (amb 1 2 3 4 5)])
             (assert (or (= 3 betty) (= 2 katty)))
             (assert (or (= 1 ethel) (= 2 john)))
             (assert (or (= 5 ethel) (= 3 john)))
             (assert (or (= 4 mary) (= 2 katty)))
             (assert (or (= 1 katty) (= 4 mary)))
             (assert (distinc betty (list ethel john katty mary)))
             (list betty ethel john katty mary))
           )
         (mutiple-dwelling)
         ))
