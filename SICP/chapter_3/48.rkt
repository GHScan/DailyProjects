#lang racket

(define (exchange account1 account2)
 'ok)

(define (serialized-exchange account1 account2)
 (if (> (account1 'id) (account2 'id))
  (serialized-exchange account2 account1)
  (let ((s1 (account1 'serializer))(s2 (account2 'serializer)))
   ((s2 (s1 exchange)))))
 )

