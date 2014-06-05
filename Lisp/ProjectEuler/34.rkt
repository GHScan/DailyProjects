#lang racket

(define (factorial n)
  (foldl * 1 (range 1 (+ n 1)))
  )

(define (find-width-m m)
  (define should-search true)
  (define result empty)
  (if (> (expt 10 (- m 1)) (* m (factorial 9)))
    (set! should-search false)
    (for-each 
      (lambda (v)
        (let iter ([m (- m 1)][number v][sum (factorial v)])
          (if (zero? m)
            (if (= number sum)
              (set! result (cons number result))
              'ok)
            (for-each 
              (lambda (v2)
                (iter (- m 1) (+ (* number 10) v2) (+ sum (factorial v2))))
              (range 0 10)))))
      (range 1 10)
      ))
  (list should-search result)
  )

;------------------------------
(define numbers
  (do ([w 3 (+ w 1)]
       [search-result (find-width-m 2) (find-width-m w)]
       [result empty (append (cadr search-result) result)])
    ((not (car search-result)) result)))

(apply + numbers)
