;------------------------------
; calling convention:
; arguments: a, b, c, d, e, ...
; ret-addr: k
; ret: val


;------------------------------
; setup
scheme-runtime-setup

(assign (reg max-free-index) (constant 1000))
(assign (reg the-cars) (op mem-alloc) (reg max-free-index))
(assign (reg the-cdrs) (op mem-alloc) (reg max-free-index))
(assign (reg new-cars) (op mem-alloc) (reg max-free-index))
(assign (reg new-cdrs) (op mem-alloc) (reg max-free-index))
(assign (reg the-free-index) (constant 0))

(goto (reg k))
;------------------------------
; cons
cons

; check gc
(test (op =) (reg max-free-index) (reg the-free-index))
(branch (label scheme-runtime-cons-call-gc))
(goto (label scheme-runtime-cons-allocate))

; call gc
scheme-runtime-cons-call-gc
(save (reg k))
(assign (reg k) (label scheme-runtime-cons-after-call-gc))
(goto (label scheme-runtime-gc))
scheme-runtime-cons-after-call-gc
(restore (reg k))

; allocate
scheme-runtime-cons-allocate
(assign (reg val) (reg the-free-index))
(assign (reg the-free-index) (op +) (reg the-free-index) (constant 1))
(perform (op mem-set!) (reg the-cars) (reg val) (reg a))
(perform (op mem-set!) (reg the-cdrs) (reg val) (reg b))

(goto (reg k))
;------------------------------
; car
car

(assign (reg val) (op mem-get) (reg the-cars) (reg a))
(goto (reg k))
;------------------------------
; cdr
cdr

(assign (reg val) (op mem-get) (reg the-cdrs) (reg a))
(goto (reg k))
;------------------------------
; set-car!
set-car!

(perform (op mem-set!) (reg the-cars) (reg a) (reg b))
(goto (reg k))
;------------------------------
; set-cdr!
set-cdr!

(perform (op mem-set!) (reg the-cdrs) (reg a) (reg b))
(goto (reg k))
;------------------------------
; gc
scheme-runtime-gc

(goto (reg k))
