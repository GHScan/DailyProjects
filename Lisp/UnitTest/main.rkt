#lang racket

(define unit-test-names '())
(define (unit-test-pushname name)
  (set! unit-test-names (cons name unit-test-names))
  )
(define (unit-test-popname)
  (set! unit-test-names (cdr unit-test-names))
  )
(define (unit-test-report b exp)
  (if b
    'ok
    (printf "failed : ~a [~a]\n" exp (reverse unit-test-names)))
  )

(define-syntax unit-test
  (lambda (x)
    (syntax-case 
      x ()
      [(unit-test name body ...) 
       (with-syntax ([id-check (datum->syntax #'unit-test 'check)]
                     [id-... (datum->syntax #'unit-test '...)])
         #'((lambda () 
              (let-syntax ([id-check
                             (syntax-rules ()
                               [(id-check e id-...) (begin (unit-test-report e 'e) id-...)]
                               )])
                (unit-test-pushname 'name)
                body ...
                (unit-test-popname)))))
       ])
    ))

(unit-test test-arithmetic
           (unit-test test-+
                      (check 
                        (= (+ 1 2) 3)
                        (= (+ 2 2) 4)
                        (= (+ 1 0) 1)) 
                      )
           (unit-test test--
                      (check
                        (= (- 1 2) (- 0 1))
                        (= (- 2 1) 1)
                        (= (- 4 2) 3))
                      )
           (unit-test test-*
                      (check
                        (= (* 1 1) 1)
                        (= (* 2 1) 2)
                        (= (* 2 1) 3)))
           )
