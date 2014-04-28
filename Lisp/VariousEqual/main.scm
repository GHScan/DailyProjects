#lang racket

(define (test-comparision name cmp)
  (printf "~a\n" name)
  (printf "\t~a: ~a\n"
          '(cmp 'a 'a)
          (cmp 'a 'a))
  (printf "\t~a: ~a\n"
          '(cmp 2 (- 5 3))
          (cmp 2 (- 5 3)))
  (printf "\t~a: ~a\n"
          '(cmp (- 5.0 3.0) (- 5.0 3.0))
          (cmp (- 5.0 3.0) (- 5.0 3.0)))
  (printf "\t~a: ~a\n"
          '(cmp (expt 2 10) (expt 2 10))
          (cmp (expt 2 10) (expt 2 10)))
  (printf "\t~a: ~a\n"
          '(cmp (expt 2 200) (expt 2 200))
          (cmp (expt 2 200) (expt 2 200)))
  (printf "\t~a: ~a\n"
          '(cmp "aaa" (make-string 3 #\a))
          (cmp "aaa" (make-string 3 #\a)))
  )

(test-comparision 'eq? eq?)
(test-comparision 'eqv? eqv?)
(test-comparision 'equal? equal?)
