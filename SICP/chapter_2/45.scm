#lang racket

(require ( planet "sicp.ss" ( "soegaard" "sicp.plt" 2 1)))

(define (split small-combine final-combine)
  (define (split-iter painter n)
    (if (zero? n)
      painter
      (let ((smaller (split-iter painter (- n 1))))
        (final-combine painter (small-combine smaller smaller))))
    )
  split-iter
  )

(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top-left (tl painter))
          (top-right (tr painter))
          (bottom-left (bl painter))
          (bottom-right (br painter)))
      (below (beside bottom-left bottom-right) (beside top-left top-right)))
    ))

(define right-split (split below beside))
(define up-split (split beside below))

(define (corner-split painter n)
  (if (zero? n)
    painter
    (let ((smaller (corner-split painter (- n 1)))
          (up (up-split painter (- n 1)))
          (right (right-split painter (- n 1))))
      (below (beside painter right) (beside up smaller))))
  )

(define (square-limit painter n) 
  (let ((combine (square-of-four flip-horiz identity
                                 rotate180 flip-vert)))
   (combine (corner-split painter n)))
 )

(paint (square-limit einstein 5))
