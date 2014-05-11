#lang racket

;------------------------------
(define (parallel-execute . procs)
  (for-each thread-wait (map thread procs))
  )

(define (make-serializer)
  (define mutex (make-mutex))
  (lambda (f)
    (lambda args
      (mutex 'acquire!)
      (apply f args)
      (mutex 'release!)
      )
    )
  )

; TODO: It should be atomic
(define (test-and-set! value)
  (let ((old (mcar value)))
    (if old
      old
      (begin (set-mcar! value true)
             old)))
  )

(define (make-mutex)
  (define value (mcons false empty))
  (define (acquire!)
    (if (test-and-set! value)
      (acquire!)
      'ok)
    )
  (define (release!)
    (set-mcar! value false)
    )
  (lambda (m)
    (cond
      ((eq? m 'acquire!) (acquire!))
      ((eq? m 'release!) (release!))
      (else (error "Invalid message for mutex: " m)))
    )
  )
;------------------------------

(define (loop n f)
  (if (> n 0)
    (begin (f)
           (loop (- n 1) f))
    'ok)
  )
;------------------------------

(let ((value 0)(s (make-serializer)))
  (define inc (s (lambda () (set! value (+ value 1)))))
  (apply parallel-execute 
         (build-list 10 (lambda (i) 
                          (lambda () (loop 10000 inc)))))
  (pretty-print value)
  )

;------------------------------

(define (make-semaphore value)
  (define value-mutex (make-mutex))
  (define schedule-mutex (make-mutex))
  (define (acquire! n)
    (value-mutex 'acquire!)
    (cond 
      ((> value n) (set! value (- value n)))
      ((= value n) (schedule-mutex 'acquire!)(set! value 0))
      ((= value 0)
       (value-mutex 'release!)
       (schedule-mutex 'acquire!)
       (schedule-mutex 'release!)
       (acquire! n))
      (else 
        (define newn (- n value))
        (schedule-mutex 'acquire!)
        (set! value 0)
        (value-mutex 'release!)
        (acquire! newn))
      )
    (value-mutex 'release!)
    )
  (define (release! n)
    (value-mutex 'acquire!)
    (if (= value 0)
      (begin (set! value n)
             (schedule-mutex 'release!))
      (set! value (+ value n)))
    (value-mutex 'release!)
    )
  (lambda (m . args)
    (cond
      ((eq? m 'acquire!) (apply acquire! args))
      ((eq? m 'release!) (apply release! args))
      (else (error "Invalid message for sempahore:" m))))
  )
