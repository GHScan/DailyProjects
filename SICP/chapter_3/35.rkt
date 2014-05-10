#lang racket

(require "33.rkt")

;------------------------------
(define (squarer a b)
  (define (on-set-value)
    (cond 
      ((a 'has-value?) (b 'set-value! (sqr (a 'get-value)) me))
      ((b 'has-value?) (a 'set-value! (sqrt (b 'get-value)) me))
      (else 'ignore))
    )
  (define (on-forget)
    (a 'forget! me)
    (b 'forget! me)
    (on-set-value)
    )
  (define (me m) 
    (cond 
      ((eq? m 'on-set-value) (on-set-value))
      ((eq? m 'on-forget) (on-forget))
      (else (error "Invalid message" m)))
    )
  (a 'connect! me)
  (b 'connect! me)
  me)

;------------------------------
(let ((a (make-connector))(b (make-connector)))
  (printf "\n------ squarer -----")
  (squarer a b)
  (probe "A" a)
  (probe "B" b)
  (a 'set-value! 4 'user)
  (a 'forget! 'user)
  (b 'set-value! 4 'user)
  )
