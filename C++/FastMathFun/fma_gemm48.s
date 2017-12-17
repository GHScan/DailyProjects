.text
    .global fma_gemm48

fma_gemm48:
    push %rbx

    mov $24,%rcx
    loop_row:

        vmovaps 0(%rdx),%ymm0
        vmovaps 32(%rdx),%ymm2
        vmovaps 64(%rdx),%ymm4
        vmovaps 96(%rdx),%ymm6
        vmovaps 128(%rdx),%ymm8
        vmovaps 160(%rdx),%ymm10
        vmovaps 192(%rdx),%ymm1
        vmovaps 224(%rdx),%ymm3
        vmovaps 256(%rdx),%ymm5
        vmovaps 288(%rdx),%ymm7
        vmovaps 320(%rdx),%ymm9
        vmovaps 352(%rdx),%ymm11
        
        mov %rsi,%rbx
        xor %rax,%rax
        loop_col:
            vbroadcastss 0(%rdi,%rax),%ymm12
            vbroadcastss 192(%rdi,%rax),%ymm13

            vmovaps 0(%rbx),%ymm14
            vfmadd231ps %ymm14,%ymm12,%ymm0
            vfmadd231ps %ymm14,%ymm13,%ymm1
            vmovaps 32(%rbx),%ymm15
            vfmadd231ps %ymm15,%ymm12,%ymm2
            vfmadd231ps %ymm15,%ymm13,%ymm3
            vmovaps 64(%rbx),%ymm14
            vfmadd231ps %ymm14,%ymm12,%ymm4
            vfmadd231ps %ymm14,%ymm13,%ymm5
            vmovaps 96(%rbx),%ymm15
            vfmadd231ps %ymm15,%ymm12,%ymm6
            vfmadd231ps %ymm15,%ymm13,%ymm7
            vmovaps 128(%rbx),%ymm14
            vfmadd231ps %ymm14,%ymm12,%ymm8
            vfmadd231ps %ymm14,%ymm13,%ymm9
            vmovaps 160(%rbx),%ymm15
            vfmadd231ps %ymm15,%ymm12,%ymm10
            vfmadd231ps %ymm15,%ymm13,%ymm11

            lea 192(%rbx),%rbx
            lea 4(%rax),%rax
            cmp $192,%rax
            jne loop_col
            
        vmovaps %ymm0,0(%rdx)
        vmovaps %ymm2,32(%rdx)
        vmovaps %ymm4,64(%rdx)
        vmovaps %ymm6,96(%rdx)
        vmovaps %ymm8,128(%rdx)
        vmovaps %ymm10,160(%rdx)
        vmovaps %ymm1,192(%rdx)
        vmovaps %ymm3,224(%rdx)
        vmovaps %ymm5,256(%rdx)
        vmovaps %ymm7,288(%rdx)
        vmovaps %ymm9,320(%rdx)
        vmovaps %ymm11,352(%rdx)

        lea 384(%rdi),%rdi
        lea 384(%rdx),%rdx
        dec %rcx
        jnz loop_row

    vzeroupper
    pop %rbx
    retq

.data
