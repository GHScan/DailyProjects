#lang racket

;------------------------------
; compare the implemention of lazy list between yield(call/cc), lambda and stream

;------------------------------
; with yield(call/cc)
(define i-eof (list 'i-eof))
(define (i-eof? v) 
  (eq? v i-eof)
  )

(define (make-iterator f)
  (let* ([main-k false]
         [iter-k false]
         [yield 
           (lambda (value)
             (call/cc (lambda (k)
                        (set! iter-k k)
                        (main-k value))))])
    (lambda ()
      (call/cc 
        (lambda (k)
          (set! main-k k)
          (if iter-k
            (iter-k)
            (begin (f yield) (yield i-eof)))))
      ))
  )

(define-syntax iterator
  (lambda (x)
    (syntax-case x ()
                 [(iterator body ...)
                  (with-syntax ([yield (datum->syntax #'iterator 'yield)])
                               #'(make-iterator (lambda (yield) body ...)))])))

(define (i->list iter)
  (let ([value (iter)])
    (if (eq? i-eof value)
      empty
      (cons value (i->list iter))))
  )

; named let
(define (i-range first last)
  (iterator 
    (let for-each ([i first])
      (if (> i last)
        'ok
        (begin (yield i) (for-each (+ i 1))))))
  )
(define (i-map proc . iters)
  (iterator 
    (let for-each ([values (map (lambda (iter) (iter)) iters)])
      (if (i-eof? (car values))
        'ok
        (begin (yield (apply proc values)) 
               (for-each (map (lambda (iter) (iter)) iters))))))
  )
(define (i-filter pred iter)
  (iterator 
    (let for-each ([value (iter)])
      (cond
        [(i-eof? value) 'ok]
        [(pred value) (yield value) (for-each (iter))]
        [else (for-each (iter))])))
  )
; do
;(define (i-range first last)
;  (iterator 
;    (do ([i first (+ i 1)]) 
;      ((> i last)) 
;      (yield i)))
;  )
;(define (i-map proc . iters)
;  (let ([iter-values (lambda () (map (lambda (iter) (iter)) iters))])
;    (iterator 
;      (do ([values (iter-values) (iter-values)]) 
;        ((i-eof? (car values))) 
;        (yield (apply proc values)))))
;  )
;(define (i-filter pred iter)
;  (iterator 
;    (do ([value (iter) (iter)])
;      ((i-eof? value))
;      (if (pred value) (yield value) value)))
;  )

;------------------------------
; with lambda
;(define i-eof (list 'i-eof))
;(define (i->list iter)
;  (let ([value (iter)])
;    (if (eq? i-eof value)
;      empty
;      (cons value (i->list iter))))
;  )
;(define (i-range first last)
;  (lambda ()
;    (if (<= first last) 
;      (let ((old first))
;        (set! first (+ first 1))
;        old)
;      i-eof))
;  )
;(define (i-map proc . iters)
;  (lambda ()
;    (let ([values (map (lambda (iter) (iter)) iters)])
;      (if (eq? i-eof (car values))
;        i-eof
;        (apply proc values))))
;  )
;(define (i-filter pred iter)
;  (letrec ([new-iter 
;             (lambda ()
;               (let ([value (iter)])
;                 (cond
;                   [(eq? i-eof value) i-eof]
;                   [(pred value) value]
;                   [else (new-iter)])))])
;    new-iter)
;  )

;------------------------------
; with stream
;(define (i->list stream)
;  (if (stream-empty? stream)
;    empty
;    (cons (stream-first stream) (i->list (stream-rest stream))))
;  )
;(define (i-range first last)
;  (if (<= first last)
;    (stream-cons first (i-range (+ first 1) last))
;    empty-stream)
;  )
;(define (i-map proc . streams)
;  (if (stream-empty? (car streams))
;    empty-stream
;    (stream-cons (apply proc (map stream-first streams)) 
;                 (apply i-map proc (map stream-rest streams))))
;  )
;(define (i-filter pred stream)
;  (cond
;    [(stream-empty? stream) empty-stream]
;    [(pred (stream-first stream)) (stream-cons (stream-first stream) (i-filter pred (stream-rest stream)))]
;    [else (i-filter pred (stream-rest stream))])
;  )

;------------------------------
; testing
(pretty-print (i->list (i-map sqr (i-filter odd? (i-range 0 10)))))
