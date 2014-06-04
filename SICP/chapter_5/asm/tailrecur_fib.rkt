main
(assign (reg a) (constant 0))
(assign (reg b) (constant 1))
(assign (reg n) (constant 20))
loop
(test (op =) (reg n) (constant 0))
(branch (label end))
(assign (reg n) (op -) (reg n) (constant 1))
(perform (op pretty-print) (reg a))
(assign (reg b) (op +) (reg a) (reg b))
(assign (reg a) (op -) (reg b) (reg a))
(goto (label loop))
end
