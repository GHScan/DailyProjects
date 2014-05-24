#lang racket
;------------------------------
(define (make-coroutine f . args)
  (letrec ([main-k false]
           [co-k false]
           [resume 
             (lambda (value)
               (call/cc 
                 (lambda (k)
                   (set! main-k k)
                   (if co-k
                     (co-k value)
                     (begin (apply f (append args (list co)))
                            (yield false))))))]
           [yield 
             (lambda (value)
               (call/cc 
                 (lambda (k)
                   (set! co-k k)
                   (main-k value))))]
           [co (lambda (m . args)
                 (cond
                   [(eq? m 'resume) (resume (car args))]
                   [(eq? m 'yield) (yield (car args))]
                   [else (error "invalid message" m)]))])
    co)
  )
(define (coroutine-resume co value)
  (co 'resume value)
  )
(define (coroutine-yield co value)
  (co 'yield value)
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
(define (sleep n co)
  (insert-event n (lambda () (coroutine-resume co false)))
  (coroutine-yield co false)
  )
(define (random-range a b)
  (+ a (random (- b a)))
  )
;------------------------------
(define the-items empty)
(define (consume-item)
  (if (empty? the-items)
    false
    (let ([item (car the-items)])
      (set! the-items (cdr the-items))
      item))
  )
(define (produce-item item)
  (set! the-items (append the-items (list item)))
  )
;------------------------------
(define (producer id loop co)
  (if (zero? loop)
    'ok
    (begin (sleep (random-range 10 20) co)
           (let ([item (random-range (* id 10) (* (+ id 1) 10))])
             (produce-item item)
             (printf "[~a][producer-~a] => ~a\n" (time) id item))
           (producer id (- loop 1) co)))
  )
(define (consumer id loop co)
  (if (zero? loop)
    'ok
    (let ([item (consume-item)])
      (if item
        (begin (printf "[~a][consumer-~a] <= ~a\n" (time) id item)
               (sleep (random-range 1 10) co))
        (sleep (random-range 10 30) co))
      (consumer id (- loop 1) co)))
  )
;------------------------------
(map (lambda (i) (coroutine-resume (make-coroutine producer i 10) false)) (range 2))
(map (lambda (i) (coroutine-resume (make-coroutine consumer i 10) false)) (range 8))
(do ([e (dispatch-event) (dispatch-event)])
  ((not e)))
