#lang racket

(require racket/match)
;------------------------------
(define (eval env e)
  (match e
         [(? symbol?) (cdr (assq e env))]
         [(list 'lambda arg-list exp-list ...) 
          (let make-lambda ([arg-list (if (empty? arg-list) '(_) arg-list)][env env])
            (lambda (arg)
              (let ([new-env (cons (cons (car arg-list) arg) env)])
                (if (empty? (cdr arg-list)) 
                  (foldl (lambda (exp _) (eval new-env exp)) empty exp-list)
                  (make-lambda (cdr arg-list) new-env)))))]
         [(list p arg-list ...) 
          (foldl (lambda (arg p) (p (eval env arg))) (eval env p) (if (empty? arg-list) '(print) arg-list))])
  )
;------------------------------
(define G (list 
           (cons 'print (lambda (n) (print ((n add1) 0))))
           (cons 'newline (lambda (_) (newline)))
           ))
;------------------------------
(eval G (read))
