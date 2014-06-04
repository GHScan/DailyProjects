#lang racket

(require "utils.rkt")
;------------------------------
(define (make-register)
  (mcons false false)
  )
(define (register-get reg)
  (mcar reg)
  )
(define (register-set! reg value)
  (set-mcar! reg value)
  )
;------------------------------
(define (make-stack max-depth)
  (let ([top empty][push-times 0][depth 0][max-depth 0])
    (lambda (m . args)
      (cond
        [(eq? m 'push) 
         (set! push-times (+ 1 push-times))
         (set! depth (+ 1 depth))
         (set! max-depth (max depth max-depth))
         (set! top (cons (car args) top))]
        [(eq? m 'pop) 
         (let ([v (car top)])
           (set! depth (- depth 1))
           (set! top (cdr top))
           v)
         ]
        [(eq? m 'print-statistics) 
         (printf "push-times=~a, max-depth=~a\n" push-times max-depth)]
        )))
  )
(define (stack-push stack v)
  (stack 'push v)
  )
(define (stack-pop stack)
  (stack 'pop)
  )
(define (stack-print-statistics stack)
  (stack 'print-statistics)
  )
;------------------------------
(define (make-machine registers operations)
  (let ([registers (map (lambda (reg-name) (cons reg-name (make-register))) registers)])
    (lambda (m . args)
      (cond
        [(eq? m 'get-register)
         (let ([kv (assoc (car args) registers)])
           (if kv
             (cdr kv)
             (error "Can't find register:" (car args))))
         ]
        [(eq? m 'get-operation) 
         (let ([kv (assoc (car args) operations)])
           (if kv
             (cdr kv)
             (error "Can't find operation: " (car args))))]
        [else (error "Invalid matchine message: " m)])))
  )
(define (machine-get-register machine reg-name)
  (machine 'get-register reg-name)
  )
(define (machine-get-operation machine op-name)
  (machine 'get-operation op-name)
  )
;------------------------------
(define (scan-registers instructions)
  (define (scan-instruction-registers instruction)
    (define (scan l)
      (cond
        [(and (pair? l) (eq? (car l) 'reg)) (list (cadr l))]
        [(pair? l) (append (scan (car l)) (scan (cdr l)))]
        [else empty])
      )
    (scan instruction)
    )
  (list->set
    (let iter ([instructions instructions][reg-names empty])
      (if (empty? instructions) 
        reg-names
        (iter (cdr instructions) (append (scan-instruction-registers (car instructions)) reg-names)))))
  )
;------------------------------
(define (make-code instruction)
  (mcons instruction false)
  )
(define (code-instruction code)
  (mcar code)
  )
(define (code-procedure code)
  (mcdr code)
  )
(define (code-set-procedure! code procedure)
  (set-mcdr! code procedure)
  )
;------------------------------
(define (assemble-code machine code label-table)
  (define (lookup-label label-name)
    (let ([kv (assq label-name label-table)])
      (if kv
        (cdr kv)
        (error "Can't find label: " label-name)))
    )
  (define (assemble-op-argument arg)
    (match arg
           [`(constant ,num-or-symbol) 
             (if (or (number? num-or-symbol) (symbol? num-or-symbol)) 'ok (error "Constant should be number or symbol: " num-or-symbol))
             (lambda () num-or-symbol)]
           [`(reg ,reg-name)
             (let ([reg (machine-get-register machine reg-name)])
               (lambda () (register-get reg)))]
           )
    )
  (code-set-procedure! 
    code
    (match 
      (code-instruction code) 
      [`(assign (reg ,target-reg) (reg ,source-reg)) 
        (let ([target-reg (machine-get-register machine target-reg)]
              [source-reg (machine-get-register machine source-reg)])
          (lambda ()
            (register-set! target-reg (register-get source-reg))))]
      [`(assign (reg ,target-reg) (constant ,num-or-symbol)) 
        (let ([target-reg (machine-get-register machine target-reg)])
          (if (or (number? num-or-symbol) (symbol? num-or-symbol)) 'ok (error "Constant should be number or symbol: " num-or-symbol))
          (lambda ()
            (register-set! target-reg num-or-symbol)))
        ]
      [`(assign (reg ,target-reg) (label ,label)) 
        (let ([target-reg (machine-get-register machine target-reg)]
              [label-code-list (lookup-label label)])
          (lambda ()
            (register-set! target-reg label-code-list)))]
      [`(assign (reg ,target-reg) (op ,op) ,args ...) 
        (let ([target-reg (machine-get-register machine target-reg)]
              [op (machine-get-operation machine op)]
              [args (map assemble-op-argument args)])
          (lambda ()
            (register-set! target-reg (apply op (map (lambda (f) (f)) args)))))]
      [`(perform (op ,op) ,args ...) 
        (let ([op (machine-get-operation machine op)]
              [args (map assemble-op-argument args)])
          (lambda ()
            (apply op (map (lambda (f) (f)) args))))
        ]
      [`(test (op ,op) ,args ...) 
        (let ([flag (machine-get-register machine 'flag)]
              [op (machine-get-operation machine op)]
              [args (map assemble-op-argument args)])
          (lambda ()
            (register-set! flag (apply op (map (lambda (f) (f)) args)))))
        ]
      [`(branch (label ,label)) 
        (let ([flag (machine-get-register machine 'flag)]
              [pc (machine-get-register machine 'pc)]
              [label-code-list (lookup-label label)])
          (lambda ()
            (if (register-get flag) 
              (register-set! pc label-code-list)
              'ok)))
        ]
      [`(goto (label ,label)) 
        (let ([pc (machine-get-register machine 'pc)]
              [label-code-list (lookup-label label)])
          (lambda ()
            (register-set! pc label-code-list)))
        ]
      [`(goto (reg ,reg-name)) 
        (let ([pc (machine-get-register machine 'pc)]
              [reg (machine-get-register machine reg-name)])
          (lambda ()
            (register-set! pc (register-get reg))))
        ]
      [`(make-stack) 
        (let ([stack (machine-get-register machine 'stack)])
          (lambda ()
            (register-set! stack (make-stack 256))))
        ]
      [`(stack-print-statistics)
        (let ([stack (machine-get-register machine 'stack)])
          (lambda ()
            (stack-print-statistics (register-get stack))))
        ]
      [`(save (reg ,reg-name)) 
        (let ([reg (machine-get-register machine reg-name)]
              [stack (machine-get-register machine 'stack)])
          (lambda ()
            (stack-push (register-get stack) (register-get reg))))
        ]
      [`(restore (reg ,reg-name)) 
        (let ([reg (machine-get-register machine reg-name)]
              [stack (machine-get-register machine 'stack)])
          (lambda ()
            (register-set! reg (stack-pop (register-get stack)))))
        ]
      ))
  code
  )
(define (make-code-list-and-label-table instructions k)
  (if (empty? instructions)
    (k empty empty)
    (make-code-list-and-label-table 
      (cdr instructions)
      (lambda (code-list label-table)
        (cond
          [(symbol? (car instructions)) 
           (let ([label-name (car instructions)])
             (if (memq label-name label-table) (error "Duplicate label: " instructions) 'ok)
             (k code-list (cons (cons label-name code-list) label-table)))]
          [else 
            (k (cons (make-code (car instructions)) code-list) label-table)]))))
  )
(define (assemble machine instructions)
  (make-code-list-and-label-table 
    instructions
    (lambda (code-list label-table)
      (map (lambda (code) (assemble-code machine code label-table)) code-list)))
  )
;------------------------------
(define builtin-operations
  (list 
    (cons 'not not)
    (cons '= =)
    (cons '< <)
    (cons '<= <=)
    (cons '> >)
    (cons '>= >=)
    (cons '+ +)
    (cons '- -)
    (cons '* *)
    (cons '/ quotient)
    (cons '% remainder)
    (cons 'mem-alloc (lambda (n) (make-vector n)))
    (cons 'mem-get (lambda (mem i) (vector-ref mem i)))
    (cons 'mem-set! (lambda (mem i v) (vector-set! mem i v)))
    (cons 'print print)
    (cons 'pretty-print pretty-print)
    (cons 'empty? empty?)
    (cons 'empty (lambda () empty))
    ))
(define builtin-registers
  '(stack flag pc)
  )
;------------------------------
(define (make-executable operations instructions)
  (let* ([machine (make-machine 
                    (append builtin-registers (scan-registers instructions))
                    (append builtin-operations operations))]
         [code-list (assemble machine instructions)]
         [pc (machine-get-register machine 'pc)])
    (lambda ()
      (register-set! pc code-list)
      (do ([code-list (register-get pc) (register-get pc)])
        ((empty? code-list))
        (register-set! pc (cdr code-list))
        ((code-procedure (car code-list))))))
  )
;------------------------------
(define (read-file fname)
  (let ([port (open-input-file fname)])
    (do ([datum (read port) (read port)][content empty (cons datum content)])
      ((eof-object? datum) (close-input-port port) (reverse content))))
  )
;------------------------------
(define inst (cons '(goto (label main))
              (apply append 
               (map (lambda (fname) (read-file fname)) 
                    (vector->list (current-command-line-arguments))))))
((make-executable empty inst))
