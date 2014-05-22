#lang racket

(define n 0)
(define (putc c)
  (cond
    ((< n 100) (set! n (+ n 1)) (display c))
    ((= n 100) (set! n (+ n 1)) (newline))
    (else 'ok))
  )

(define (make-yang-continuation yin)
  (lambda (yang) 
    (putc #\*) 
    (yin yang))
  )

(define yin-continuation
  (lambda (yin)
    (putc #\@)
    (let ((yang-continuation (make-yang-continuation yin)))
      (yang-continuation yang-continuation)))
  )

(yin-continuation yin-continuation)


; use subsituation mode to reduction
; y is short for yin-continuation, m is short for make-yang-continuation
; (y x) can be reduce, and output @; ((m x1) x2) can be reduce and output *
;
;   (y y) =>
;   
;   @ ((m y) (m y)) =>
;   @* (y (m y)) =>
;   
;   @* @ ((m (m y)) (m (m y))) =>
;   @* @* ((m y) (m (m y))) =>
;   @* @** (y (m (m y))) =>
;   
;   @* @** @ ((m (m (m y))) (m (m (m y)))) =>
;   @* @** @* ((m (m y)) (m (m (m y)))) =>
;   @* @** @** ((m y) (m (m (m y)))) =>
;   @* @** @*** (y (m (m (m y)))) =>
;   
;   ...
