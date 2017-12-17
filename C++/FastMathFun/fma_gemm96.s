.text
    .global fma_gemm96

fma_gemm96:
    push %rbx

    mov $32,%rcx
    loop_row:

        vmovaps 0(%rdx),%ymm0
        vmovaps 32(%rdx),%ymm1
        vmovaps 64(%rdx),%ymm2
        vmovaps 96(%rdx),%ymm3
        vmovaps 384(%rdx),%ymm4
        vmovaps 416(%rdx),%ymm5
        vmovaps 448(%rdx),%ymm6
        vmovaps 480(%rdx),%ymm7
        vmovaps 768(%rdx),%ymm8
        vmovaps 800(%rdx),%ymm9
        vmovaps 832(%rdx),%ymm10
        vmovaps 864(%rdx),%ymm11
        
        mov %rsi,%rbx
        xor %rax,%rax
        loop_col1:
            vbroadcastss 0(%rdi,%rax),%ymm12
            vbroadcastss 384(%rdi,%rax),%ymm13
            vbroadcastss 768(%rdi,%rax),%ymm14

            prefetcht0 1152(%rbx)
            prefetcht0 1216(%rbx)

            vmovaps 0(%rbx),%ymm15
            vfmadd231ps %ymm15,%ymm12,%ymm0
            vfmadd231ps %ymm15,%ymm13,%ymm4
            vfmadd231ps %ymm15,%ymm14,%ymm8
            vmovaps 32(%rbx),%ymm15
            vfmadd231ps %ymm15,%ymm12,%ymm1
            vfmadd231ps %ymm15,%ymm13,%ymm5
            vfmadd231ps %ymm15,%ymm14,%ymm9
            vmovaps 64(%rbx),%ymm15
            vfmadd231ps %ymm15,%ymm12,%ymm2
            vfmadd231ps %ymm15,%ymm13,%ymm6
            vfmadd231ps %ymm15,%ymm14,%ymm10
            vmovaps 96(%rbx),%ymm15
            vfmadd231ps %ymm15,%ymm12,%ymm3
            vfmadd231ps %ymm15,%ymm13,%ymm7
            vfmadd231ps %ymm15,%ymm14,%ymm11

            lea 384(%rbx),%rbx
            lea 4(%rax),%rax
            cmp $384,%rax
            jne loop_col1
            
        vmovaps %ymm0,0(%rdx)
        vmovaps %ymm1,32(%rdx)
        vmovaps %ymm2,64(%rdx)
        vmovaps %ymm3,96(%rdx)
        vmovaps %ymm4,384(%rdx)
        vmovaps %ymm5,416(%rdx)
        vmovaps %ymm6,448(%rdx)
        vmovaps %ymm7,480(%rdx)
        vmovaps %ymm8,768(%rdx)
        vmovaps %ymm9,800(%rdx)
        vmovaps %ymm10,832(%rdx)
        vmovaps %ymm11,864(%rdx)

        vmovaps 128(%rdx),%ymm0
        vmovaps 160(%rdx),%ymm1
        vmovaps 192(%rdx),%ymm2
        vmovaps 224(%rdx),%ymm3
        vmovaps 512(%rdx),%ymm4
        vmovaps 544(%rdx),%ymm5
        vmovaps 576(%rdx),%ymm6
        vmovaps 608(%rdx),%ymm7
        vmovaps 896(%rdx),%ymm8
        vmovaps 928(%rdx),%ymm9
        vmovaps 960(%rdx),%ymm10
        vmovaps 992(%rdx),%ymm11
        
        mov %rsi,%rbx
        xor %rax,%rax
        loop_col2:
            vbroadcastss 0(%rdi,%rax),%ymm12
            vbroadcastss 384(%rdi,%rax),%ymm13
            vbroadcastss 768(%rdi,%rax),%ymm14

            prefetcht0 1280(%rbx)
            prefetcht0 1344(%rbx)

            vmovaps 128(%rbx),%ymm15
            vfmadd231ps %ymm15,%ymm12,%ymm0
            vfmadd231ps %ymm15,%ymm13,%ymm4
            vfmadd231ps %ymm15,%ymm14,%ymm8
            vmovaps 160(%rbx),%ymm15
            vfmadd231ps %ymm15,%ymm12,%ymm1
            vfmadd231ps %ymm15,%ymm13,%ymm5
            vfmadd231ps %ymm15,%ymm14,%ymm9
            vmovaps 192(%rbx),%ymm15
            vfmadd231ps %ymm15,%ymm12,%ymm2
            vfmadd231ps %ymm15,%ymm13,%ymm6
            vfmadd231ps %ymm15,%ymm14,%ymm10
            vmovaps 224(%rbx),%ymm15
            vfmadd231ps %ymm15,%ymm12,%ymm3
            vfmadd231ps %ymm15,%ymm13,%ymm7
            vfmadd231ps %ymm15,%ymm14,%ymm11

            lea 384(%rbx),%rbx
            lea 4(%rax),%rax
            cmp $384,%rax
            jne loop_col2
            
        vmovaps %ymm0,128(%rdx)
        vmovaps %ymm1,160(%rdx)
        vmovaps %ymm2,192(%rdx)
        vmovaps %ymm3,224(%rdx)
        vmovaps %ymm4,512(%rdx)
        vmovaps %ymm5,544(%rdx)
        vmovaps %ymm6,576(%rdx)
        vmovaps %ymm7,608(%rdx)
        vmovaps %ymm8,896(%rdx)
        vmovaps %ymm9,928(%rdx)
        vmovaps %ymm10,960(%rdx)
        vmovaps %ymm11,992(%rdx)

        vmovaps 256(%rdx),%ymm0
        vmovaps 288(%rdx),%ymm1
        vmovaps 320(%rdx),%ymm2
        vmovaps 352(%rdx),%ymm3
        vmovaps 640(%rdx),%ymm4
        vmovaps 672(%rdx),%ymm5
        vmovaps 704(%rdx),%ymm6
        vmovaps 736(%rdx),%ymm7
        vmovaps 1024(%rdx),%ymm8
        vmovaps 1056(%rdx),%ymm9
        vmovaps 1088(%rdx),%ymm10
        vmovaps 1120(%rdx),%ymm11
        
        mov %rsi,%rbx
        xor %rax,%rax
        loop_col3:
            vbroadcastss 0(%rdi,%rax),%ymm12
            vbroadcastss 384(%rdi,%rax),%ymm13
            vbroadcastss 768(%rdi,%rax),%ymm14

            prefetcht0 1408(%rbx)
            prefetcht0 1472(%rbx)

            vmovaps 256(%rbx),%ymm15
            vfmadd231ps %ymm15,%ymm12,%ymm0
            vfmadd231ps %ymm15,%ymm13,%ymm4
            vfmadd231ps %ymm15,%ymm14,%ymm8
            vmovaps 288(%rbx),%ymm15
            vfmadd231ps %ymm15,%ymm12,%ymm1
            vfmadd231ps %ymm15,%ymm13,%ymm5
            vfmadd231ps %ymm15,%ymm14,%ymm9
            vmovaps 320(%rbx),%ymm15
            vfmadd231ps %ymm15,%ymm12,%ymm2
            vfmadd231ps %ymm15,%ymm13,%ymm6
            vfmadd231ps %ymm15,%ymm14,%ymm10
            vmovaps 352(%rbx),%ymm15
            vfmadd231ps %ymm15,%ymm12,%ymm3
            vfmadd231ps %ymm15,%ymm13,%ymm7
            vfmadd231ps %ymm15,%ymm14,%ymm11

            lea 384(%rbx),%rbx
            lea 4(%rax),%rax
            cmp $384,%rax
            jne loop_col3
            
        vmovaps %ymm0,256(%rdx)
        vmovaps %ymm1,288(%rdx)
        vmovaps %ymm2,320(%rdx)
        vmovaps %ymm3,352(%rdx)
        vmovaps %ymm4,640(%rdx)
        vmovaps %ymm5,672(%rdx)
        vmovaps %ymm6,704(%rdx)
        vmovaps %ymm7,736(%rdx)
        vmovaps %ymm8,1024(%rdx)
        vmovaps %ymm9,1056(%rdx)
        vmovaps %ymm10,1088(%rdx)
        vmovaps %ymm11,1120(%rdx)

        lea 1152(%rdi),%rdi
        lea 1152(%rdx),%rdx
        dec %rcx
        jnz loop_row

    vzeroupper
    pop %rbx
    retq

.data
