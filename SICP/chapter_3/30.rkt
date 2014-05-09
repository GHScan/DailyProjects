#lang racket

(require "32.rkt")

;------------------------------
(define (ripple-carry-adder a-list b-list cin s-list cout)
  (let ((a (car a-list))(b (car b-list))(s (car s-list)))
    (if (= 1 (length a-list))
      (full-adder a b cin s cout)
      (let ((tmp-cout (make-wire)))
        (full-adder a b cin s tmp-cout)
        (ripple-carry-adder (cdr a-list) (cdr b-list) tmp-cout (cdr s-list) cout))
      ))
  )

(define (make-wires n)
  (build-list n (lambda (i) (make-wire)))
  )

(define (write-number-to-wires wires number)
  (if (null? wires)
    true
    (begin (set-signal! (car wires) (= 1 (remainder number 2)))
           (write-number-to-wires (cdr wires) (quotient number 2))))
  )
(define (read-number-from-wires wires)
  (foldr (lambda (wire value) (+ (* 2 value) (if (get-signal wire) 1 0))) 0 wires)
  )

;------------------------------
(display "\n---------- exercise-30 -----------\n")

(define a-bits (make-wires 32))
(define b-bits (make-wires 32))
(define s-bits (make-wires 32))
(define cin (make-wire))
(define cout (make-wire))
(ripple-carry-adder a-bits b-bits cin s-bits cout)
(printf "\n@done: ~a\n" (propagate))

(define (add-bit32 a b)
  (printf "\n @add-bit32: ~a + ~a = " a b)
  (write-number-to-wires a-bits a)
  (write-number-to-wires b-bits b)
  (agenda-set-current-time! the-agenda 0)
  (propagate)
  (display (read-number-from-wires s-bits))
  (printf ", ~a delay" (agenda-current-time the-agenda))
  )

(add-bit32 1 1)
(add-bit32 19 54)
(add-bit32 223 472)
(add-bit32 1024 1025)
(add-bit32 4096 4100)
(add-bit32 16384 16400)
(add-bit32 65535 65536)
