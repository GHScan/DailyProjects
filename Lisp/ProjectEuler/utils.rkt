#lang racket

(provide (all-defined-out))

;------------------------------
(define (divisible? n m)
  (= 0 (remainder n m))
  )
;------------------------------
; stream
(define (stream-merge key . streams)
  (if (empty? (cdr streams))
    (car streams)
    (let ([a (car streams)][b (apply stream-merge key (cdr streams))])
      (cond
        [(= (key (stream-first a)) (key (stream-first b))) (stream-cons (stream-first a) (stream-merge key (stream-rest a) (stream-rest b)))]
        [(< (key (stream-first a)) (key (stream-first b))) (stream-cons (stream-first a) (stream-merge key (stream-rest a) b))]
        [else (stream-cons (stream-first b) (stream-merge key a (stream-rest b)))])))
  )

(define (stream-take s n)
  (if (= 0 n)
    empty-stream
    (stream-cons (stream-first s) (stream-take (stream-rest s) (- n 1))))
  )

(define (stream-take-until s proc)
  (if (proc (stream-first s))
    empty-stream
    (stream-cons (stream-first s) (stream-take-until (stream-rest s) proc)))
  )

(define (stream-map proc . streams)
  (stream-cons (apply proc (map stream-first streams))
               (apply stream-map proc (map stream-rest streams)))
  )

(define (stream-last s)
  (if (stream-empty? (stream-rest s))
    (stream-first s)
    (stream-last (stream-rest s)))
  )
;------------------------------
; memoization
(define (memoize f)
  (let ([cache (make-hash)])
    (lambda args
      (let ([v (hash-ref cache args false)])
        (if v
          v
          (begin (set! v (apply f args))
                 (hash-set! cache args v)
                 v)))))
  )

;------------------------------
; primes
(define prime-list (stream-cons 2 (stream-filter prime? (in-naturals 3))))
(define (prime? n)
  (let iter ([prime-list prime-list][max-i (floor (sqrt n))])
    (if (> (stream-first prime-list) max-i)
      true
      (and (not (= 0 (remainder n (stream-first prime-list)))) (iter (stream-rest prime-list) max-i))))
  )

(define (find-prime-factors n)
  (let iter ([prime-list prime-list][n n][result empty])
    (let ([prime (stream-first prime-list)])
      (cond
        [(= 1 n) result]
        [(> (* prime prime) n) (cons n result)]
        [(= 0 (remainder n prime)) (iter prime-list (quotient n prime) (cons prime result))]
        [else (iter (stream-rest prime-list) n result)]))
    )
  )

(define (find-prime-factors-pair n)
  (let iter ([prime-list prime-list][n n][result empty])
    (let ([prime (stream-first prime-list)])
      (cond
        [(= n 1) result]
        [(> (* prime prime) n) (cons (cons n 1) result)]
        [(= 0 (remainder n prime)) 
         (do ([count 1 (+ count 1)][n (quotient n prime) (quotient n prime)])
           ((not (= 0 (remainder n prime))) (iter (stream-rest prime-list) n (cons (cons prime count) result))))]
        [else (iter (stream-rest prime-list) n result)]))
    )
  )
;------------------------------
; list compreshension
(define (flatten-map proc l)
  (apply append (map proc l))
  )

(define-syntax list-comprehension
  (syntax-rules (<-)
    [(_ exp) (list exp)]
    [(_ exp (x <- alist) rules ...) (flatten-map (lambda (x) (list-comprehension exp rules ...)) alist)]
    [(_ exp filter rules ...) (if filter (list-comprehension exp rules ...) empty)]
    )
  )
