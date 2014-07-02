(define (time-it f)
  (define start (current-inexact-milliseconds))
  (f)
  (pretty-print (- (current-inexact-milliseconds) start))
  )

(define (jmp-loop n)
  (label start)
  (if (> n 0)
    (begin (set! n (- n 1)) (goto start))
    n)
  )

(define (recur-loop n)
 (if (> n 0) (recur-loop (- n 1)) n)
  )

(time-it (lambda () (jmp-loop 1000000)))
(time-it (lambda () (recur-loop 1000000)))
