(require ( planet "sicp.ss" ( "soegaard" "sicp.plt" 2 1)))

(define right-split 
  (lambda (painter n)
    (cond 
      ((= 0 n) painter)
      (else
        ((lambda (smaller) 
           (beside painter (below smaller smaller))
           )
         (right-split painter (- n 1)))))
    ))
(define up-split 
  (lambda (painter n)
    (cond
      ((= 0 n) painter)
      (else 
        ((lambda (smaller) 
           (below painter (beside smaller smaller)))
         (up-split painter (- n 1)))))
    ))

(define corner-split
  (lambda (painter n)
    (cond
      ((= 0 n) painter)
      (else 
        ((lambda (smaller top-left bottom-right) 
           (below (beside painter bottom-right) (beside top-left smaller)))
         (corner-split painter (- n 1))
         (up-split painter (- n 1))
         (right-split painter (- n 1))
         )))
    ))

(define square-limit
  (lambda (painter n)
    ((lambda (top-right)
       ((lambda (bottom-right)
          (below (beside (flip-horiz bottom-right) bottom-right) (beside (flip-horiz top-right) top-right))
          )
        (flip-vert top-right))
       )
     (corner-split painter n))
    ))

(paint (square-limit einstein 5))
