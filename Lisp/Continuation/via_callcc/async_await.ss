#lang racket
;------------------------------
(define (async proc . args)
  (letrec ([callback (last args)]
           [main-k false]
           [task-k false]
           [await 
             (lambda (f args)
               (apply f (append args 
                                (list 
                                  (lambda (value)
                                    (call/cc 
                                      (lambda (k)
                                        (set! main-k k)
                                        (task-k value)))
                                    ))))
               (call/cc (lambda (k)
                          (set! task-k k)
                          (main-k false))))]
           [task 
             (lambda (m . args)
               (cond
                 [(eq? m 'await) (await (car args) (cadr args))]
                 [else (error "invalid message:" m)]))])
    (call/cc 
      (lambda (k)
        (set! main-k k)
        (callback (apply proc (append (drop-right args 1) (list task))))
        (main-k false)
        )))
  )
(define (await task f . args)
  (task 'await f args)
  )
;------------------------------
(define _now 0)
(define _events empty)
(define (time)
  _now
  )
(define (dispatch-event)
  (if (empty? _events)
    false
    (let ([e (car _events)])
      (set! _events (cdr _events))
      (set! _now (car e))
      ((cdr e))
      true))
  )
(define (insert-event d callback)
  (set! _events 
    (let insert ([finish (+ d (time))][events _events])
      (cond
        [(empty? events) (list (cons finish callback))]
        [(< finish (caar events)) (cons (cons finish callback) events)]
        [else (cons (car events) (insert finish (cdr events)))])))
  )
;------------------------------
(define (random-range a b)
  (+ a (random (- b a)))
  )
(define (connect host port callback)
  (insert-event (random-range 5 10) (lambda () (callback "conn")))
  )
(define (send conn data)
  data)
(define (receive conn callback)
  (insert-event (random-range 1 4) (lambda () (callback (string-append "response " (number->string (time))))))
  )
(define (close conn)
  conn)
;------------------------------
(define (request-url url port task)
  (let ([conn (await task connect url port)])
    (printf "[~a]~a: connected\n" (time) url)
    (send conn "GET / HTTP/1.0")
    (do ([i 1 (+ 1 i)][tmp (await task receive conn) (await task receive conn)])
      ((> i 10))
      (printf "[~a]~a: receive => ~a\n" (time) url tmp) (send conn tmp))
    (let ([html (await task receive conn)])
      (printf "[~a]~a: receive html => ~a\n" (time) url html)
      (close conn)
      (printf "[~a]~a: close\n" (time) url)
      html))
  )
(define (request-urls-sync urls task)
  (do ([_urls urls (cdr _urls)])
    ((empty? _urls))
    (await task async request-url (car _urls) 80))
  )
(define (request-urls-async urls callback)
  (let* ([n 0]
         [request-callback 
           (lambda (html)
             (set! n (+ n 1))
             (if (= n (length urls))
               (callback true)
               'ok))])
    (do ([_urls urls (cdr _urls)])
      ((empty? _urls))
      (async request-url (car _urls) 80 request-callback)))
  )
;------------------------------
(define urls '("www.baidu.com" "www.qq.com" "www.taobao.com" "www.sina.com"))
;(async request-urls-sync urls (lambda (v) (printf "finish all\n")))
(request-urls-async urls (lambda (v) (printf "finish all\n")))
(do ([e (dispatch-event) (dispatch-event)]) ((not e)))
