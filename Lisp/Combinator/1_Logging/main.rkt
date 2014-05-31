#lang racket

(require racket/date)
;------------------------------
(define (log-file fname)
  (lambda (level message)
    (printf "[file:~a]: ~a- ~a\n" fname level message))
  )
(define (log-timestamp logger)
 (lambda (level message)
  (printf "[~a]" (date->string (current-date)))
  (logger level message))
 )
(define (log-backtracking logger)
 (lambda (level message)
  (logger level message)
  (printf "\t\t Stack traceback\n"))
 )
(define (log+ . loggers)
  (lambda (level message)
    (do ((loggers loggers (cdr loggers)))
      ((empty? loggers))
      ((car loggers) level message)))
  )
(define (log* logger . wrappers)
  (do ([logger logger ((car wrappers) logger)][wrappers wrappers (cdr wrappers)])
   ((empty? wrappers) logger))
  )
(define (_log? pred logger)
  (lambda (level message)
    (if (pred level message)
      (logger level message)
      (void)))
  )
(define-syntax log?
  (lambda (x)
    (syntax-case x (level message)
      [(log? pred e) 
       (with-syntax ([level (datum->syntax #'log? 'level)]
                     [message (datum->syntax #'log? 'message)])
         #'(_log? (lambda (level message) pred) e))]))
  )

;------------------------------
(define log-level-debug 0)
(define log-level-verbose 1)
(define log-level-warning 2)
(define log-level-error 3)

(define log 
  (log+ 
    (log? (= level log-level-debug) (log-file "console"))
    (log? (= level log-level-verbose) (log-file "verbose"))
    (log? (= level log-level-warning) (log-file "warning"))
    (log? (= level log-level-error) 
          (log+ 
            (log* (log-file "error") log-timestamp log-backtracking)
            (log-file "console")))
    ))

(log log-level-debug "this is debug message.")
(log log-level-verbose "this is verbose message.")
(log log-level-warning "this is warning message.")
(log log-level-error "this is error message.")
