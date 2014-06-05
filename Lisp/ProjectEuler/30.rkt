#lang racket

(define (find-width-m-pow-e e m)
  (define should-search true)
  (define result empty)
  (if (> (expt 10 (- m 1)) (* m (expt 9 e)))
    (set! should-search false)
    (for-each 
      (lambda (v)
        (let iter ([m (- m 1)][number v][sum (expt v e)])
          (if (zero? m)
            (if (= number sum)
              (set! result (cons number result))
              'ok)
            (for-each 
              (lambda (v2)
                (iter (- m 1) (+ (* number 10) v2) (+ sum (expt v2 e))))
              (range 0 10)))))
      (range 1 10)
      ))
  (list should-search result)
  )

;------------------------------
(define numbers
  (do ([w 2 (+ w 1)]
       [search-result (find-width-m-pow-e 5 2) (find-width-m-pow-e 5 w)]
       [result empty (append (cadr search-result) result)])
    ((not (car search-result)) result)))

(apply + numbers)
