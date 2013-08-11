#include "pch.h"

#include <stdlib.h>
#include <stdio.h>
#include <time.h>

#ifndef __GNUC__
#define __SSE__ 1
#endif

#define TIMING(stmts) {clock_t start = clock(); stmts; printf("%f\n", float(clock() - start) / CLOCKS_PER_SEC);}

#define MEM_ALIGNMENT 16
#ifdef __GNUC__
#define ALIGN(n)  __attribute__((aligned(n)))
static void* align_malloc(int size, int align) {
    void* p;
    int err = posix_memalign(&p, align, size);
    if (err != 0) return NULL;
    return p;
}
#else
#define ALIGN(n)  __declspec(align(n))
#define align_malloc _aligned_malloc
#endif

struct ALIGN(MEM_ALIGNMENT) Matrix {
    float v[4][4]; 
    void transpose() {
        for (int i = 0; i < 4; ++i) {
            for (int j = 0; j <= i; ++j) {
                std::swap(v[i][j], v[j][i]);
            }
        }
    }
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

#ifdef __SSE__
#include <xmmintrin.h>
void transform2(Vector *outputs, int n, const Vector *inputs, const Matrix* _mat) {
	Matrix mat(*_mat);
	mat.transpose();

	__m128 m[4], t[4];
	for (int i = 0; i < 4; ++i) m[i] = _mm_load_ps(mat.v[i]);

	for (int i = 0; i < n; ++i) {
		Vector *out = outputs + i;
		const Vector *in = inputs + i;

		for (int i = 0; i < 4; ++i) {
			t[i] = _mm_set_ss(in->v[i]);
			t[i] = _mm_mul_ps(t[i], m[i]);
		}
		t[0] = _mm_add_ps(t[0], t[1]);
		t[2] = _mm_add_ps(t[2], t[3]);
		t[0] = _mm_add_ps(t[0], t[2]);
		_mm_store_ps(out->v, t[0]);
	}
}
#endif

int main() {
    const int LOOP = 1 << 13;
    const int N = 1 << 14;
    Vector *inputs = (Vector*)align_malloc(N * sizeof(Vector), 16);
    Vector *outputs = (Vector*)align_malloc(N * sizeof(Vector), 16);
    Matrix mat = {1, 1.1, 1.2, 1.3, 2, 2.1, 2.2, 2.3, 3.1, 3.2, 0, 0, 4.1};

    puts("C++");
    TIMING(
        for (int i = 0; i < LOOP; ++i) {
            transform(outputs, N, inputs, &mat);
        }
    );

#ifdef __SSE__
    puts("xmminstrin:");
    TIMING(
        for (int i = 0; i < LOOP; ++i) {
            transform2(outputs, N, inputs, &mat);
        }
    );
#endif
}
