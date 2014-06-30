(define Vec
  (class (x y z)
         (define (length)
           (sqrt (sqr-length)))
         (define (sqr-length)
           (+ (+ (* x x) (* y y)) (* z z)))
         )
  )

(define (Vec-sqr-length v)
  (+ (+ (* (field v x) (field v x))
        (* (field v y) (field v y)))
     (* (field v z) (field v z)))
  )

(define (make-Vec2 x y z)
  (define (length)
    (sqrt (sqr-length))
    )
  (define (sqr-length)
    (+ (+ (* x x) (* y y)) (* z z))
    )
  (lambda (m)
    (cond
      [(eq? m 'x) x]
      [(eq? m 'y) y]
      [(eq? m 'z) z]
      [(eq? m 'length) length]
      [(eq? m 'sqr-length) sqr-length]
      [else (error "invalid message:" m)]))
  )

(define (Vec2-sqr-length v)
  (+ (+ (* (v 'x) (v 'x))
        (* (v 'y) (v 'y)))
     (* (v 'z) (v 'z)))
  )

;------------------------------
(define v (Vec 1 2 3))
(pretty-print (field v x))
(pretty-print (field v y))
(pretty-print ((method v sqr-length)))
(pretty-print ((method v length)))

(define v2 (make-Vec2 1 2 3))
(pretty-print (v2 'x))
(pretty-print (v2 'y))
(pretty-print ((v2 'sqr-length)))
(pretty-print ((v2 'length)))
;------------------------------
(define (time-it f)
  (define start (current-inexact-milliseconds))
  (f)
  (pretty-print (- (current-inexact-milliseconds) start))
  )

(define (loop f n)
  (define i 0)
  (label start)
  (if (< i n)
    (begin (set! i (+ i 1)) (f) (goto start))
    n)
  )
;------------------------------
(define N 100000)
(time-it (lambda ()
           (let ([v (Vec 2 3 4)])
             (loop (method v sqr-length) N))))
(time-it (lambda ()
           (let ([v (Vec 2 3 4)])
             (loop (lambda () (Vec-sqr-length v)) N))))
(time-it (lambda ()
           (let ([v (make-Vec2 2 3 4)])
             (loop (v 'sqr-length) N))))
(time-it (lambda ()
           (let ([v (make-Vec2 2 3 4)])
             (loop (lambda () (Vec2-sqr-length v)) N))))
