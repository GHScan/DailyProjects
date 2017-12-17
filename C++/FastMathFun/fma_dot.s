.text
.global fma_dot

fma_dot:
    shl $2,%rdx
    xor %rcx,%rcx

    vxorps %ymm0,%ymm0,%ymm0
    vxorps %ymm2,%ymm2,%ymm2
    vxorps %ymm4,%ymm4,%ymm4
    vxorps %ymm6,%ymm6,%ymm6
    vxorps %ymm8,%ymm8,%ymm8
    vxorps %ymm10,%ymm10,%ymm10
    vxorps %ymm12,%ymm12,%ymm12
    vxorps %ymm14,%ymm14,%ymm14

    loop:
        vmovaps 0(%rsi,%rcx),%ymm1
        vmovaps 32(%rsi,%rcx),%ymm3
        vmovaps 64(%rsi,%rcx),%ymm5
        vmovaps 96(%rsi,%rcx),%ymm7
        vmovaps 128(%rsi,%rcx),%ymm9
        vmovaps 160(%rsi,%rcx),%ymm11
        vmovaps 192(%rsi,%rcx),%ymm13
        vmovaps 224(%rsi,%rcx),%ymm15
        vfmadd231ps 0(%rdi,%rcx),%ymm1,%ymm0
        vfmadd231ps 32(%rdi,%rcx),%ymm3,%ymm2
        vfmadd231ps 64(%rdi,%rcx),%ymm5,%ymm4
        vfmadd231ps 96(%rdi,%rcx),%ymm7,%ymm6
        vfmadd231ps 128(%rdi,%rcx),%ymm9,%ymm8
        vfmadd231ps 160(%rdi,%rcx),%ymm11,%ymm10
        vfmadd231ps 192(%rdi,%rcx),%ymm13,%ymm12
        vfmadd231ps 224(%rdi,%rcx),%ymm15,%ymm14

        lea 256(%rcx),%rcx
        cmp %rcx,%rdx
        jne loop

    vaddps %ymm0,%ymm2,%ymm0
    vaddps %ymm4,%ymm6,%ymm4
    vaddps %ymm8,%ymm10,%ymm8
    vaddps %ymm12,%ymm14,%ymm12
    vaddps %ymm0,%ymm4,%ymm0
    vaddps %ymm8,%ymm12,%ymm8
    vaddps %ymm0,%ymm8,%ymm0

    /* TODO: horizontally sum ymm0 */
    vzeroupper
    retq

.data
