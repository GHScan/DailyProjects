#lang racket

(define (primes max-n)
  (define (iter source)
    (if (stream-empty? source)
      empty
      (cons (stream-first source)
            (iter (stream-filter (lambda (i) (> (remainder i (stream-first source)) 0)) (stream-rest source)))))
    )
  (iter (in-range 2 max-n))
  )

(primes 100)
