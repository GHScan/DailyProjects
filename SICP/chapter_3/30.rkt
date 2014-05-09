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
  (foldl (lambda (wire value) (set-signal! wire (= 1 (remainder value 2))) (quotient value 2)) number wires)
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

(define (bit32-add a b)
  (printf "\n @add-bit32: ~a + ~a = " a b)
  (write-number-to-wires a-bits a)
  (write-number-to-wires b-bits b)
  (agenda-set-current-time! the-agenda 0)
  (propagate)
  (display (read-number-from-wires s-bits))
  (printf ", ~a delay" (agenda-current-time the-agenda))
  )

(bit32-add 1 1)
(bit32-add 19 54)
(bit32-add 223 472)
(bit32-add 1024 1025)
(bit32-add 4096 4100)
(bit32-add 16384 16400)
(bit32-add 65535 65536)
