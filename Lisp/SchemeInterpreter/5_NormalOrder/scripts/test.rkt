(define ones (cons 1 ones))
(define integers (cons 1 (map-2 + ones integers)))
(define fibs (cons 1 (cons 2 (map-2 + fibs (cdr fibs)))))

(pretty-print (take fibs 60))
