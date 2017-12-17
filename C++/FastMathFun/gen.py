
def gen_gemm48():
    print('.text')
    print('    .global gemm48_gen')

    print('gemm48_gen:')
    print('    mov $24,%rcx')
    print('    loop_row:')

    for i in range(6):
        print('        vmovaps %d(%%rdx),%%ymm%d' % (i * 32, i * 2))
        print('        vmovaps %d(%%rdx),%%ymm%d' % (192 + i * 32, 1 + i * 2))

    for i in range(48):
        print('            vbroadcastss %d(%%rdi),%%ymm12' % (i * 4))
        print('            vbroadcastss %d(%%rdi),%%ymm13' % (i * 4 + 192))
        for j in range(6):
            print('            vmovaps %d(%%rsi),%%ymm14' % (i * 192 + j * 32))
            print('            vfmadd231ps %%ymm14,%%ymm12,%%ymm%d' % (j * 2))
            print('            vfmadd231ps %%ymm14,%%ymm13,%%ymm%d' % (1 + j * 2))
                
    for i in range(6):
        print('        vmovaps %%ymm%d,%d(%%rdx)' % (i * 2, i * 32))
        print('        vmovaps %%ymm%d,%d(%%rdx)' % (1 + i * 2, 192 + i * 32))

    print('        lea 384(%rdi),%rdi')
    print('        lea 384(%rdx),%rdx')
    print('        dec %rcx')
    print('        jnz loop_row')

    print('    vzeroupper')
    print('    retq')

    print('.data')

def gen_gemm96():
    print('.text')
    print('    .global gemm96_gen')

    print('gemm96_gen:')

    print('    mov $32,%rcx')
    print('    loop_row:')

    for c in range(3):
        for i in range(3):
            for j in range(4):
                print('        vmovaps %d(%%rdx),%%ymm%d' % (i * 384 + j * 32 + c * 128, i * 4 + j))

        for i in range(96):
            print('            vbroadcastss %d(%%rdi),%%ymm12' % (i * 4))
            print('            vbroadcastss %d(%%rdi),%%ymm13' % (384 + i * 4))
            print('            vbroadcastss %d(%%rdi),%%ymm14' % (768 + i * 4))

            print('            prefetcht0 %d(%%rsi)' % ((i + 3) * 384 + 128 * c))
            print('            prefetcht0 %d(%%rsi)' % ((i + 3) * 384 + 64 + 128 * c))

            for j in range(4):
                print('            vmovaps %d(%%rsi),%%ymm15' % (j * 32 + i * 384 + 128 * c))
                print('            vfmadd231ps %%ymm15,%%ymm12,%%ymm%d' % (j))
                print('            vfmadd231ps %%ymm15,%%ymm13,%%ymm%d' % (4 + j))
                print('            vfmadd231ps %%ymm15,%%ymm14,%%ymm%d' % (8 + j))
                    
        for i in range(3):
            for j in range(4):
                print('        vmovaps %%ymm%d,%d(%%rdx)' % (i * 4 + j, i * 384 + j * 32 + c * 128))

    print('        lea 1152(%rdi),%rdi')
    print('        lea 1152(%rdx),%rdx')
    print('        dec %rcx')
    print('        jnz loop_row')

    print('    vzeroupper')
    print('    retq')

    print('.data')

def gen_transform_cols(working_set):
    print('.text')
    print('    .global _transform_cols_{0}_gen'.format(working_set))

    print('_transform_cols_{0}_gen:'.format(working_set))
    print('    push %rbx')

    print('    shl $2,%rcx')
    print('    lea (%rsi,%rcx),%rbx')
    print('    loop_col:')

    for i in range(2):
        for j in range(6):
            print('        vxorps %ymm{0},%ymm{0},%ymm{0}'.format(j * 2))
            print('        vxorps %ymm{0},%ymm{0},%ymm{0}'.format(j * 2 + 1))

        for j in range(4):
            if j == 0:
                print('        mov %rsi,%rax')
            else:
                print('        lea (%rax,%rcx,1),%rax')

            if i == 0:
                for k in range(3):
                    if working_set == "L2":
                        print('        prefetcht0 {0}(%rax)'.format(192 * 1 + k * 64))
                    if working_set == "L3":
                        print('        prefetcht0 {0}(%rax)'.format(192 * 2 + k * 64))
                    if working_set == "Memory":
                        print('        prefetchnta {0}(%rax)'.format(192 * 4 + k * 64))
            
            print('        vbroadcastss {0}(%rdi),%ymm12'.format((i * 8 + j) * 4))
            print('        vbroadcastss {0}(%rdi),%ymm13'.format((i * 8 + 4 + j) * 4))
            for k in range(6):
                print('        vmovaps {0}(%rax),%ymm14'.format(k * 32))
                print('        vfmadd231ps %ymm14,%ymm12,%ymm{0}'.format(k * 2))
                print('        vfmadd231ps %ymm14,%ymm13,%ymm{0}'.format(k * 2 + 1))

        store_ins = 'vmovaps' if working_set == 'L1' else 'vmovntps'
        if i == 0:
            print('        mov %rdx,%rax')
        else:
            print('        lea (%rdx,%rcx,2),%rax')
        for j in range(6):
            print('        {0} %ymm{1},{2}(%rax)'.format(store_ins, j * 2, j * 32))
        print('        lea (%rax,%rcx,1),%rax')
        for j in range(6):
            print('        {0} %ymm{1},{2}(%rax)'.format(store_ins, j * 2 + 1, j * 32))

    print('        lea 192(%rsi),%rsi')
    print('        lea 192(%rdx),%rdx')
    print('        cmp %rsi,%rbx')
    print('        jnz loop_col')

    if working_set != 'L1':
        print('    sfence')
    print('    vzeroupper')
    print('    pop %rbx')
    print('    retq')

    print('.data')

def gen_transform_cols_L1():
    gen_transform_cols('L1')
def gen_transform_cols_L2():
    gen_transform_cols('L2')
def gen_transform_cols_L3():
    gen_transform_cols('L3')
def gen_transform_cols_Memory():
    gen_transform_cols('Memory')

if __name__ == '__main__':
    import sys
    globals()[sys.argv[1]]()
