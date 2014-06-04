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
      (list n (stream-first a) (- n (stream-first a)))
      (iter (stream-rest a))))
  )

(define (goldbach-list lower upper)
  (map goldbach (filter even? (range lower upper)))
  )

(define (goldbach-list-over-limit lower upper limit)
  (filter (lambda (v) (> (cadr v) limit)) (goldbach-list lower upper))
  )

(define (print-goldbach-list l)
  (for-each (lambda (v) (printf "~a =~a + ~a\n" (car v) (cadr v) (caddr v))) l)
  )
;------------------------------
(print-goldbach-list (goldbach-list 9 21))
(newline)
(print-goldbach-list (goldbach-list-over-limit 1 2000 50))
