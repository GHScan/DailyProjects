#lang racket
(require scheme/mpair)

(define (make-table same-key?)
  (define table (mlist 'table))
  (define (assoc records key)
    (cond 
      ((null? records) false)
      ((same-key? key (mcar (mcar records))) (mcar records))
      (else (assoc (mcdr records) key)))
    )
  (define (lookup key)
    (let ((record (assoc (mcdr table) key)))
      (if record 
        (mcdr record)
        false))
    )
  (define (insert! key value)
    (let ((record (assoc (mcdr table) key)))
      (if record
        (set-mcdr! record value)
        (set-mcdr! table
                   (mcons (mcons key value)
                          (mcdr table)))))
    )
  (lambda (m . args)
    (cond
      ((eq? m 'lookup) (apply lookup args))
      ((eq? m 'insert!) (apply insert! args))
      (else (error "invalid message!")))
    )
  )

;
(define (float-equal-p1 a b)
  (< (abs (- a b)) 0.1)
  )

(define t1 (make-table float-equal-p1))
(t1 'insert! 1.0 0)
(t1 'insert! 1.2 2)
(t1 'insert! 1.3 3)
(t1 'insert! 1.32 3.2)
(t1 'lookup 1)
(t1 'lookup 1.2)
(t1 'lookup 1.3)
(t1 'lookup 1.41)
(t1 'lookup 1.5)
