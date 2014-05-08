#lang racket

(define (make-monitored f)
  (define count 0)
  (lambda args
    (if (and (= 1 (length args)) (symbol? (car args)))
      (cond
        ((eq? (car args) 'reset-count) (set! count 0) count)
        ((eq? (car args) 'how-many-calls?) count)
        (else (error "invalid message!" args)))
      (begin (set! count (+ 1 count))
             (apply f args)))
    ))

(define s (make-monitored sqrt))
(s 10)
(s 100)
(s 'how-many-calls?)
(s 'reset-count)
(s 'how-many-calls?)
(s 9)
(s 'how-many-calls?)
