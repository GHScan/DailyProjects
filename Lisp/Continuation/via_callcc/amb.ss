#lang racket

;------------------------------
; amb       => nondeterministic branch
; with-amb  => new amb block, at most max-n possible result will be return as a list
; amb-set!  => set value, but will undo after backtracking

(define the-amb-nested-backtrackings empty)
(define (amb-push-backtracking! backtracking)
  (set! the-amb-nested-backtrackings (mcons backtracking the-amb-nested-backtrackings))
  )
(define (amb-pop-backtracking!)
  (set! the-amb-nested-backtrackings (mcdr the-amb-nested-backtrackings))
  )
(define (amb-top-backtracking)
  (mcar the-amb-nested-backtrackings)
  )
(define (amb-set-top-backtracking! backtracking)
  (set-mcar! the-amb-nested-backtrackings backtracking)
  )

(define (with-amb max-n f . args)
  (let* ([main-k false]
         [value-list empty]
         [backtracking (lambda ()
                         (amb-pop-backtracking!)
                         (main-k 'backtracking))])
    (amb-push-backtracking! backtracking)
    (let ([state (call/cc 
                   (lambda (k)
                     (set! main-k k)
                     (k 'install)))])
      (cond
        [(eq? state 'install) 
         (set! value-list (cons (apply f args) value-list))
         (if (< (length value-list) max-n)
           (amb)
           (backtracking))]
        [(eq? state 'backtracking) 
         (reverse value-list)]
        [else (error "invalid state:" state)])))
  )

(define (do-amb . exps)
  (call/cc 
    (lambda (k)
      (let iterate ([rest exps])
        (if (empty? rest)
          'ok
          (begin (iterate (cdr rest))
                 (let ([old-backtracking (amb-top-backtracking)])
                   (amb-set-top-backtracking! 
                     (lambda ()
                       (amb-set-top-backtracking! old-backtracking)
                       (k ((car rest)))))))))
      ((amb-top-backtracking))))
  )

(define (amb-register-undo undo)
  (let ([old-backtacking (amb-top-backtracking)])
    (amb-set-top-backtracking! 
      (lambda ()
        (undo)
        (old-backtacking))))
  )

(define-syntax amb
  (syntax-rules ()
                [(amb e ...) (do-amb (lambda () e) ...)])
  )

(define-syntax amb-set!
  (syntax-rules ()
                [(amb-set! var e) 
                 (let ([old-v var]) 
                   (amb-register-undo (lambda () (set! var old-v)))
                   (set! var e))])
  )

;------------------------------
(define (assert b)
  (if b b (amb))
  )
(define (a-integer-between first last)
  (assert (<= first last))
  (amb first (a-integer-between (+ first 1) last))
  )
(define (a-integer-from n)
  (amb n (a-integer-from (+ n 1)))
  )
(define (one-of l)
  (assert (not (empty? l)))
  (amb (car l) (one-of (cdr l)))
  )
(define (one-of-permutation l)
  (if (empty? (cdr l))
    l
    (let ([rest-perm (one-of-permutation (cdr l))])
      (let insert-between ([left empty][right rest-perm])
        (if (empty? right)
          (append left (list (car l)))
          (amb (append left (cons (car l) right)) 
               (insert-between (append left (list (car right))) (cdr right)))))))
  )
(define (set-insert set v)
  (if (member v set)
    set
    (cons v set))
  )
;------------------------------
; testing

; i^2 + j^2 = k^2
(define (a-pythagorean-triple)
  (let* ([k (a-integer-from 1)]
         [j (a-integer-between 1 (- k 1))]
         [max-i (inexact->exact (sqrt (- (* k k) (* j j))))]
         [i (a-integer-between 1 max-i)])
    (assert (= (+ (* i i) (* j j)) (* k k)))
    (list i j k))
  )

; a super point-24
(define (a-point-n nums sum)
  (define (gen-ops n)
    (if (zero? n) 
      empty
      (cons (one-of '(+ - * /)) (gen-ops (- n 1))))
    )
  (define (combine-exp rvalue rexp op num)
    (if (< num rvalue)
      (list op num rexp)
      (list op rexp num))
    )
  (let ([num-perm (one-of-permutation nums)][ops (gen-ops (- (length nums) 1))])
    (let build-exp ([nums (cdr num-perm)][ops ops][rvalue (car num-perm)][rexp (car num-perm)])
      (if (empty? nums)
        (begin (assert (= sum rvalue)) rexp)
        (cond
          [(eq? '+ (car ops)) (build-exp (cdr nums) (cdr ops) (+ rvalue (car nums)) (combine-exp rvalue rexp (car ops) (car nums)))]
          [(eq? '- (car ops)) (build-exp (cdr nums) (cdr ops) (- rvalue (car nums)) (combine-exp rvalue rexp (car ops) (car nums)))]
          [(eq? '* (car ops)) (build-exp (cdr nums) (cdr ops) (* rvalue (car nums)) (combine-exp rvalue rexp (car ops) (car nums)))]
          [(eq? '/ (car ops)) (build-exp (cdr nums) (cdr ops) (/ rvalue (car nums)) (combine-exp rvalue rexp (car ops) (car nums)))]))))
  )

; queens 8
(define (a-queens-n n)
  (define (check-board row col board row-board)
    (cond 
      [(zero? row-board) 'ok]
      [(or (= col (car board)) (= (- row row-board) (abs (- col (car board))))) (amb)]
      [else (check-board row col (cdr board) (- row-board 1))])
    )
  (define (a-board i)
    (if (zero? i)
      empty
      (let ([small-board (a-board (- i 1))][col (a-integer-between 1 n)])
        (check-board i col small-board (- i 1))
        (cons col small-board)))
    )
  (a-board n)
  )

(define (main)
  ; (a-pythagorean-triple)
  ; (a-point-n '(3 4 5 6) 24)
   (a-queens-n 8)
  )

;------------------------------
;
(define rlist (with-amb 1000 main))
(do ([l rlist (cdr l)][set empty (set-insert set (car l))])
  ((empty? l) set))
