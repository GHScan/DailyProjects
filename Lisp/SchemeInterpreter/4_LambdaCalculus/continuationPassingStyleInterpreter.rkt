#lang racket

(require racket/match)
;------------------------------
(define (eval env e k)
  (match e
         [(? symbol?) (k (cdr (assq e env)))]
         [(list 'lambda arg-list exp-list ...)
          (let ([arg-list (if (empty? arg-list) '(_) arg-list)])
            (cond
              [(> (length arg-list) 1) (eval env `(lambda (,(car arg-list)) (lambda ,(cdr arg-list) ,@exp-list)) k)]
              [(> (length exp-list) 1) (eval env `(lambda ,arg-list ((lambda (_) ,@(cdr exp-list)) ,(car exp-list))) k)]
              [else (k (lambda (arg k2) (eval (cons (cons (car arg-list) arg) env) (car exp-list) k2)))]))]
         [(list p arg-list ...)
          (let ([arg-list (if (empty? arg-list) '(print) arg-list)])
            (if (= 1 (length arg-list))
              (eval env p (lambda (p) 
                            (eval env (car arg-list) (lambda (arg)
                                                       (p arg k)))))
              (eval env `((,p ,(car arg-list)) ,@(cdr arg-list)) k)))]
         )
  )
;------------------------------
(define G (list 
            (cons 'print (lambda (n k) 
                           (n (lambda (v k2)
                                (k2 (add1 v))) 
                              (lambda (v) 
                                (v 0 (lambda (v) (k (print v))))))))
            (cons 'newline (lambda (_ k)
                             (k (newline))))
            (cons 'call/cc (lambda (f k)
                             (f (lambda (v k2) (k v)) k)))
            )) 
;------------------------------
(eval G (read) identity)
