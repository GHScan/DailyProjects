(define n 0)
(define (putc c)
  (cond
   [(< n 100) (set! n (+ n 1)) (display c)]
   [(= n 100) (set! n (+ n 1)) (display c)]
   [else (exit)])
 )

(let ([yin ((lambda (cc) (putc 1) cc) (call/cc (lambda (c) c)))])
  (let ([yang ((lambda (cc) (putc 0) cc) (call/cc (lambda (c) c)))])
    (yin yang)))
