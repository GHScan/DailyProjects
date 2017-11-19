
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

    print('    retq')

    print('.data')

if __name__ == '__main__':
    import sys
    globals()[sys.argv[1]]()
