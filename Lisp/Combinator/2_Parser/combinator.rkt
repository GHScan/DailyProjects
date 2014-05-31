#lang racket

(provide (all-defined-out))
;------------------------------
(define (c-atom t)
  (lambda (input sk fk)
    (if (and (not (empty? input)) (equal? (car input) t))
      (sk (car input) (cdr input))
      (fk input)))
  )

(define (c-atom-in l)
  (lambda (input sk fk)
    (if (and (not (empty? input)) (member (car input) l)) 
      (sk (car input) (cdr input))
      (fk input)))
  )

(define (c-atom-not parser)
  (lambda (input sk fk)
    (parser input 
            (lambda (v _) (fk input)) 
            (lambda (_) 
              (if (not (empty? input))
                (sk (car input) (cdr input))
                (fk input)))))
  )
    
(define (c-atom-list s)
  (lambda (input sk fk)
    (let iter ([rest-s s][rest-input input])
      (cond
        [(empty? rest-s) (sk s rest-input)]
        [(empty? rest-input) (fk input)]
        [(equal? (car rest-s) (car rest-input)) (iter (cdr rest-s) (cdr rest-input))]
        [else (fk input)])))
  )

(define c-eof
  (lambda (input sk fk) 
    (if (empty? input) 
      (sk empty input)
      (fk input))))

(define c-empty
  (lambda (input sk fk)
    (sk empty input))
  )

(define (c-and . parsers)
  (lambda (input sk fk)
    (let iter ([parsers parsers][values empty][rest-input input])
      (if (empty? parsers)
        (sk (reverse values) rest-input)
        ((car parsers) rest-input 
                       (lambda (value rest-input)
                         (iter (cdr parsers) (cons value values) rest-input))
                       (lambda (_) (fk input)))))
    )
  )

(define (c-or . parsers)
  (lambda (input sk fk)
    (let iter ([parsers parsers])
      (if (empty? parsers)
        (fk input)
        ((car parsers) input sk 
                       (lambda (_) (iter (cdr parsers)))))))
  )

(define (c-select parser proc)
  (lambda (input sk fk)
    (parser input (lambda (v rest) (sk (proc v) rest)) fk))
  )

(define (c-repeat parser)
  (lambda (input sk fk)
    (let iter ([values empty][input input])
      (parser input 
              (lambda (value rest-input) (iter (cons value values) rest-input))
              (lambda (rest-input) 
                (if (not (empty? values))
                  (sk (reverse values) rest-input)
                  (fk rest-input)))))
    )
  )

(define (c-repeat0 parser)
  (c-or (c-repeat parser) c-empty)
  )

(define (c-make-lazy-parser thunk)
  (let ([parser false])
    (lambda (input sk fk)
      (if parser 'ok (set! parser (thunk)))
      (parser input sk fk)))
  )
(define-syntax c-lazy 
  (syntax-rules ()
    [(_ name e) (define name (c-make-lazy-parser (lambda () e)))]
    )
  )
