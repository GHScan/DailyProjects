'x
'(lambda (x g40) (g40 x))
'(lambda (x g42) (x 1 g42))
'(f x (lambda (g43) (if g43 a b)))
'(if x (f a (lambda (g45) g45)) b)
'(lambda (x g49) (f x (lambda (g47) (if g47 (g49 a) (g49 b)))))
'(lambda (x g53)
   (let ((g54 (lambda (g51) (if g51 (g53 c) (g53 d)))))
     (if x (f a g54) (g54 b))))
'(lambda (x g57)
   (let ((g58 (lambda (g55) (if g55 (g57 c) (g57 d)))))
     (if x (g58 (zero? a)) (g58 b))))
'(lambda (x g62) (if t (if x (f a g62) (g62 b)) (g62 c)))
'(lambda (x g67)
   (let ((g68 (lambda (g65) (if g65 (g67 e) (g67 w)))))
     (if t (if x (f a g68) (g68 b)) (g68 c))))
'(lambda (x g72)
   (let ((g73 (lambda (g70) (h g70 g72)))) (if x (f a g73) (g73 b))))
'(lambda (x g77)
   (let ((g78 (lambda (g75) (g75 c g77)))) (if x (f g g78) (g78 h))))
'(f
  a
  (lambda (g79)
    (g
     b
     (lambda (g80)
       (g79
        g80
        (lambda (g81)
          (f
           c
           (lambda (g82)
             (g
              d
              (lambda (g83)
                (g82 g83 (lambda (g84) (g81 g84 (lambda (g85) g85))))))))))))))
'(lambda (n g95)
   ((lambda (fact g88) (fact fact (lambda (g86) (g86 n g88))))
    (lambda (fact g93)
      (g93
       (lambda (n g92)
         (if (zero? n)
           (g92 1)
           (fact
            fact
            (lambda (g89) (g89 (sub1 n) (lambda (g90) (g92 (* n g90))))))))))
    g95))
