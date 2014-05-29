(map (lambda (i) (* i i)) 
     (filter (lambda (i) (= 0 (remainder i 2))) 
             (build-list 10 identity)))

(define (fib n)
  (if (<= n 1)
    1
    (+ (fib (- n 1)) (fib (- n 2))))
  )
(build-list 10 fib)

(let ((x 1))
  (let ((y 2))
    (set! x (+ y 1))
    (set! y (+ x 1))
    (or (> x y) (cons (cons x y) '(1 2 3)))))

(map (curry 2 * 2) (build-list 10 identity))

(eval '(foldl (curry 3 + 1) 0 (build-list 10 identity)))
