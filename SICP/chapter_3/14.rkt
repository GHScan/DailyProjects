#lang racket

(define (mreverse l)
  (define (iter n newlist)
    (if (null? n)
      newlist
      (let ((next (mcdr n)))
        (begin (set-mcdr! n newlist)
               (iter next n))
        ))
    )
  (iter l empty)
  )

(define v (mcons 'a (mcons 'b (mcons 'c (mcons 'd empty)))))
(display v)(newline)
(define w (mreverse v))
(display v)(newline)
(display w)(newline)
