#lang racket

(define prime-list (stream-cons 2 (stream-filter is-prime? (in-naturals 3))))
(define (is-prime? n)
  (let iter ([max-i (floor (sqrt n))][prime-list prime-list])
    (if (> (stream-first prime-list) max-i)
      true
      (and (not (= 0 (remainder n (stream-first prime-list)))) (iter max-i (stream-rest prime-list))))
    )
  )

(define (goldbach n)
  (let iter ([a prime-list])
    (if (is-prime? (- n (stream-first a)))
      (list n `(,(stream-first a) ,(- n (stream-first a))))
      (iter (stream-rest a))))
  )
;------------------------------
(map goldbach (filter even? (range 3 64)))
