;------------------------------
; argument: n, ret
; return: val
fib
(test (op <=) (reg n) (constant 1))
(branch (label fib-01))
(save (reg ret))

fib-n-1
(save (reg n))
(assign (reg ret) (label fib-n-2))
(assign (reg n) (op -) (reg n) (constant 1))
(goto (label fib))

fib-n-2
(restore (reg n))
(assign (reg n) (op -) (reg n) (constant 2))
(save (reg val))
(assign (reg ret) (label fib-n-2-after))
(goto (label fib))

fib-n-2-after
(restore (reg n))
(restore (reg ret))
(assign (reg val) (op +) (reg val) (reg n))
(goto (reg ret))

fib-01
(assign (reg val) (reg n))
(goto (reg ret))
;------------------------------
; 
main
(make-stack)
(assign (reg n) (constant 0))

; loop, print fib(0)~fib(19)
main-loop
(test (op =) (reg n) (constant 20))
(branch (label main-loop-end))

(assign (reg ret) (label main-after-fib))
(save (reg n))
(goto (label fib))
main-after-fib
(restore (reg n))
(perform (op pretty-print) (reg val))

(assign (reg n) (op +) (reg n) (constant 1))
(goto (label main-loop))

;
main-loop-end
(stack-print-statistics)
