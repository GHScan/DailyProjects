'x
'(lambda (x g39) (g39 x))
'(lambda (x g41) (x 1 g41))
'(f x (lambda (g42) (if g42 a b)))
'(if x (f a (lambda (g44) g44)) b)
'(lambda (x g48) (f x (lambda (g46) (if g46 (g48 a) (g48 b)))))
'(lambda (x g52)
   (let ((g53 (lambda (g50) (if g50 (g52 c) (g52 d)))))
     (if x (f a g53) (g53 b))))
'(lambda (x g56)
   (let ((g57 (lambda (g54) (if g54 (g56 c) (g56 d)))))
     (if x (g57 (zero? a)) (g57 b))))
'(lambda (x g61) (if t (if x (f a g61) (g61 b)) (g61 c)))
'(lambda (x g66)
   (let ((g67 (lambda (g64) (if g64 (g66 e) (g66 w)))))
     (if t (if x (f a g67) (g67 b)) (g67 c))))
'(lambda (x g71)
   (let ((g72 (lambda (g69) (h g69 g71)))) (if x (f a g72) (g72 b))))
'(lambda (x g76)
   (let ((g77 (lambda (g74) (g74 c g76)))) (if x (f g g77) (g77 h))))
'(f
  a
  (lambda (g78)
    (g
     b
     (lambda (g79)
       (g78
        g79
        (lambda (g80)
          (f
           c
           (lambda (g81)
             (g
              d
              (lambda (g82)
                (g81 g82 (lambda (g83) (g80 g83 (lambda (g84) g84))))))))))))))
'(lambda (n g94)
   ((lambda (fact g87) (fact fact (lambda (g85) (g85 n g87))))
    (lambda (fact g92)
      (g92
       (lambda (n g91)
         (if (zero? n)
           (g91 1)
           (fact
            fact
            (lambda (g88) (g88 (sub1 n) (lambda (g89) (g91 (* n g89))))))))))
    g94))
