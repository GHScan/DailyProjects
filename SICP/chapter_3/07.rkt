#lang racket

(define (make-account balance init-passwd)
  (define (withdraw value)
    (if (>= balance value)
      (begin (set! balance (- balance value))
             balance)
      "Insufficient funds")
    )   
  (define (deposit value)
    (set! balance (+ balance value))
    balance
    )

  (lambda (passwd message)
    (if (eq? passwd init-passwd)
      (cond
        ((eq? message 'withdraw) withdraw)
        ((eq? message 'deposit) deposit))
      (lambda args "Invalid password"))
    ))

(define (make-joint account oldpasswd newpasswd) 
  (lambda (passwd message)
    (if (eq? passwd newpasswd)
      (account oldpasswd message)
      (lambda args "Invalid passwd 2"))
    )
  )

(define acc (make-account 100 '123))
((acc '123 'withdraw) 10)
((acc '234 'deposit) 20)
((acc '123 'deposit) 20)

(define acc2 (make-joint acc '123 '456))
((acc2 '123 'withdraw) 10)
((acc2 '456 'withdraw) 10)
((acc2 '456 'deposit) 20)
