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
			t[i] = _mm_set_ps1(in->v[i]);
			t[i] = _mm_mul_ps(t[i], m[i]);
		}
		t[0] = _mm_add_ps(t[0], t[1]);
		t[2] = _mm_add_ps(t[2], t[3]);
		t[0] = _mm_add_ps(t[0], t[2]);
		_mm_store_ps(out->v, t[0]);
	}
}
#endif


void transform3(Vector *outputs, int n, const Vector *inputs, const Matrix* _mat) {
	Matrix mat(*_mat);
	mat.transpose();

	__asm {
		movaps xmm4, mat.v
		movaps xmm5, mat.v + 16
		movaps xmm6, mat.v + 32
		movaps xmm7, mat.v + 48
	}

	for (int i = 0; i < n; ++i) {
		float *_out = outputs[i].v;
		const float *_in = inputs[i].v;

        __asm {
            push eax
            mov eax, _in
            movss xmm0, [eax]
            shufps xmm0, xmm0, 0
            movss xmm1, [eax+4]
            shufps xmm1, xmm1, 0
            movss xmm2, [eax+8]
            shufps xmm2, xmm2, 0
            movss xmm3, [eax+12]
            shufps xmm3, xmm3, 0
            mulps xmm0, xmm4
            mulps xmm1, xmm5
            mulps xmm2, xmm6
            mulps xmm3, xmm7
            addps xmm0, xmm1
            addps xmm0, xmm2
            addps xmm0, xmm3
            movaps _out, xmm0
            pop eax
        }
	}
}

void initInputs(Vector *inputs, int n) {
    for (int i = 0; i < n; ++i) {
        inputs[i].v[0] = float(1 + i);
        inputs[i].v[1] = float(2 + i);
        inputs[i].v[2] = float(4 + i);
    }
}
float getSumOfOutputs(Vector *outputs, int n) {
    float ret = 0;
    for (int i = 0; i < n; ++i) {
        ret += outputs[i].v[0] + outputs[i].v[1] + outputs[i].v[2];
    }
    return ret;
}

int main() {
    const int LOOP = 1 << 13;
    const int N = 1 << 14;
    Vector *inputs = (Vector*)align_malloc(N * sizeof(Vector), 16);
    Vector *outputs = (Vector*)align_malloc(N * sizeof(Vector), 16);
    Matrix mat = {1.0f, 1.1f, 1.2f, 1.3f, 2.f, 2.1f, 2.2f, 2.3f, 3.1f, 3.2f, 0, 0, 4.1f};
    float ret, ret2;

    puts("C++");
    initInputs(inputs, N);
    TIMING(
        for (int i = 0; i < LOOP; ++i) {
            transform(outputs, N, inputs, &mat);
        }
    );
    ret = getSumOfOutputs(outputs, N);

#ifdef __SSE__
    puts("xmminstrin:");
    initInputs(inputs, N);
    TIMING(
        for (int i = 0; i < LOOP; ++i) {
            transform2(outputs, N, inputs, &mat);
        }
    );
    ret2 = getSumOfOutputs(outputs, N);
    if (ret != ret2) puts("failed !!!");
#endif

    puts("sse instrin:");
    initInputs(inputs, N);
    TIMING(
        for (int i = 0; i < LOOP; ++i) {
            transform3(outputs, N, inputs, &mat);
        }
    );
    ret2 = getSumOfOutputs(outputs, N);
    if (ret != ret2) puts("failed !!!");
}
