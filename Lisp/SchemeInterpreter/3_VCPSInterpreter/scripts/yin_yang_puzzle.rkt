(define n 0)
(define (putc c)
  (cond
   [(< n 100) (set! n (+ n 1)) (display c)]
   [(= n 100) (set! n (+ n 1)) (display c)]
   [else (exit)])
 )

(let* ((yin ((lambda (cc) (putc "@") cc) (call/cc (lambda (c) c))))   
       (yang ((lambda (cc) (putc "*") cc) (call/cc (lambda (c) c))))) 
  (yin yang))
