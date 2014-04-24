#lang racket
(require racket/pretty)

(define (fast-prime? n)
  (define (even n) (= 0 (remainder n 2)))
  (define (div a b) (floor (/ a b)))
  (define (square a) (* a a))
  (define (exp-mod a b m)
    (cond
      ((= b 0) 
       1)
      ((even b)
       (remainder (square (exp-mod a (div b 2) m)) m))
      (else 
        (remainder (* a (exp-mod a (- b 1) m)) m)
        ))
    )
  (define (fermat-test n)
    (define (try-it a n) (= a (exp-mod a n n)))
    (try-it (+ 1 (random (- n 1))) n)
    )
  (define (test-prime-times n times)
    (cond 
      ((<= times 0) true)
      ((fermat-test n) (test-prime-times n (- times 1)))
      (else false)
      )
    )

  (test-prime-times n 20)
  )

(define (search-for-primes start end)
  (define (report-number-time n elapse-time)
    (newline)
    (display n)
    (display " *** ")
    (display elapse-time)
    )
  (define (test-prime-and-print n start-time) 
    (cond 
      ((fast-prime? n)
       (report-number-time n (- (current-inexact-milliseconds) start-time))
       ))
    )

  (test-prime-and-print start (current-inexact-milliseconds))

  (cond
    ((< start end) 
     (search-for-primes (+ start 1) end)
     ))
  )

(search-for-primes 10007 10037)
(search-for-primes 100003 100043)
(search-for-primes 1000003 1000037)
(search-for-primes 10000019 10000103)
