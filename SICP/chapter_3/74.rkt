#lang racket

(provide (all-defined-out))

(define (stream-map proc . streams)
  (if (ormap stream-empty? streams)
    empty-stream
    (stream-cons (apply proc (map stream-first streams))
                 (apply stream-map proc (map stream-rest streams)))
    )
  )

(define (sign-change-detector s-old s-new)
  (if (<= (* s-new s-old) 0)
    (cond 
      ((> s-new 0) 1)
      ((< s-new 0) -1)
      (else (if (< s-old 0) 1 -1)))
    0)
  )

(define (make-zero-crossings input-stream last-value)
  (if (stream-empty? input-stream)
    empty-stream
    (stream-cons (sign-change-detector last-value (stream-first input-stream))
                 (make-zero-crossings (stream-rest input-stream) (stream-first input-stream))))
  )

(define (make-zero-crossings2 input-stream last-value)
  (stream-map sign-change-detector (stream-cons last-value input-stream) input-stream)
  )

(define sense-data (stream 1 2 1.5 1 0.5 -0.1 -2 -3 -2 -0.5 0.2 3 4))
(define zero-crossing (make-zero-crossings sense-data 0))
(define zero-crossing2 (make-zero-crossings2 sense-data 0))

(stream->list zero-crossing)
(stream->list zero-crossing2)
