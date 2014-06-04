main

; call scheme-runtime-setup
(assign (reg k) (label main-after-scheme-runtime-setup))
(goto (label scheme-runtime-setup))
main-after-scheme-runtime-setup

; build a 10 item list
(assign (reg alist) (op empty))
(assign (reg n) (constant 10))

main-loop-build-list-10
(test (op =) (reg n) (constant 0))
(branch (label main-loop-print-list-10))

(assign (reg k) (label main-after-call-cons))
(assign (reg a) (reg n))
(assign (reg b) (reg alist))
(goto (label cons))
main-after-call-cons
(assign (reg alist) (reg val))
(assign (reg n) (op -) (reg n) (constant 1))
(goto (label main-loop-build-list-10))

; print every thing
main-loop-print-list-10
(test (op empty?) (reg alist))
(branch (label main-end))

(assign (reg k) (label main-after-call-car))
(assign (reg a) (reg alist))
(goto (label car))
main-after-call-car
(perform (op pretty-print) (reg val))

(assign (reg k) (label main-after-call-cdr))
(goto (label cdr))
main-after-call-cdr
(assign (reg alist) (reg val))

(goto (label main-loop-print-list-10))

; end
main-end
