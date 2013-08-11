#include "pch.h"

#include <stdlib.h>
#include <stdio.h>
#include <time.h>

#define TIMING(stmts) {clock_t start = clock(); stmts; printf("%f\n", float(clock() - start) / CLOCKS_PER_SEC);}

#define MEM_ALIGNMENT 16
#ifdef __GNUC__
#define ALIGN(n)  __attribute__((aligned(n)))
static void* align_malloc(int size, int align) {
    void* p;
    posix_memalign(&p, align, size);
    return p;
}
#else
#define ALIGN(n)  __declspec(align(n))
#define align_malloc _aligned_malloc
#endif

struct ALIGN(MEM_ALIGNMENT) Matrix {
    float v[4][4];
} ;
struct ALIGN(MEM_ALIGNMENT) Vector {
    float v[4];
};

void transform(Vector *outputs, int n, const Vector *inputs, const Matrix* mat) {
    for (int i = 0; i < n; ++i) {
        Vector *out = outputs + i;
        const Vector *in = inputs + i;
        for (int i = 0; i < 4; ++i) {
            out->v[i] = mat->v[i][0] * in->v[0]
                + mat->v[i][1] * in->v[1]
                + mat->v[i][2] * in->v[2]
                + mat->v[i][3] * in->v[3];
        }
    }
}

int main() {
    const int LOOP = 1 << 13;
    const int N = 1 << 14;
    Vector *inputs = (Vector*)align_malloc(N * sizeof(Vector), 16);
    Vector *outputs = (Vector*)align_malloc(N * sizeof(Vector), 16);
    Matrix mat = {1, 1.1, 1.2, 1.3, 2, 2.1, 2.2, 2.3, 3.1, 3.2, 0, 0, 4.1};

    TIMING(
        for (int i = 0; i < LOOP; ++i) {
            transform(outputs, N, inputs, &mat);
        }
    );
}
