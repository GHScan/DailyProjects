#lang racket

(define the-exception-catch-continues empty)
(define (exception-push-continue! k)
  (set! the-exception-catch-continues (cons k the-exception-catch-continues))
  )
(define (exception-pop-continue!)
  (let ([k (car the-exception-catch-continues)])
    (set! the-exception-catch-continues (cdr the-exception-catch-continues))
    k)
  )
(define (throw e)
  ((exception-pop-continue!) (cons 'throw e))
  )
(define (do-try-catch try-clause catch-clause)
  (let ([state (call/cc (lambda (k)
                          (exception-push-continue! k)
                          (k (cons 'try empty))))])
    (cond 
      [(eq? (car state) 'try) (try-clause) (exception-pop-continue!)]
      [(eq? (car state) 'throw)  (catch-clause (cdr state))]
      [else (error "invalid state" state)]))
  )

(define-syntax try
  (syntax-rules 
    (catch)
    [(try try-clause catch var-e catch-clause ...) 
     (do-try-catch 
       (lambda () try-clause) 
       (lambda (var-e) 
         (cond catch-clause ... [else (throw var-e)])))]))

;------------------------------
(define (test-throw n)
  (if (> n 5)
    (throw n)
    'ok)
  )
(define (test-loop)
  (do ([i 0 (+ i 1)])
    ((> i 10))
    (try 
      (test-throw i)
      catch e
      [(< e 8) (printf "~a : caught exception : ~a\n" test-loop e)]))
  )
(define (test-main)
  (try
    (test-loop)
    catch e
    [true (printf "~a : caught exception: ~a\n" test-main e)])
  )
(test-main)
