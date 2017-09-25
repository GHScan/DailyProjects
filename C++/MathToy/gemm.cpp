#include <immintrin.h>

void sgemm_avx_48_48_48(float *a, float *b, float *c)
{
    for (size_t i = 0; i < 48; i += 8) {
        for (size_t k = 0; k < 48; k += 8) {
            __m256 sum0 = _mm256_setzero_ps();
            __m256 sum1 = _mm256_setzero_ps();
            __m256 sum2 = _mm256_setzero_ps();
            __m256 sum3 = _mm256_setzero_ps();
            __m256 sum4 = _mm256_setzero_ps();
            __m256 sum5 = _mm256_setzero_ps();
            __m256 sum6 = _mm256_setzero_ps();
            __m256 sum7 = _mm256_setzero_ps();
            for (size_t j = 0; j < 48; ++j) {
                __m256 b_jk = _mm256_load_ps(b + j * 48 + k);

                __m256 a0_ij = _mm256_set1_ps(*(a + i * 48 + j));
                __m256 a1_ij = _mm256_set1_ps(*(a + i * 48 + 48 + j));
                __m256 mul0 = _mm256_mul_ps(b_jk, a0_ij);
                __m256 mul1 = _mm256_mul_ps(b_jk, a1_ij);
                sum0 = _mm256_add_ps(sum0, mul0);
                sum1 = _mm256_add_ps(sum1, mul1);

                a0_ij = _mm256_set1_ps(*(a + i * 48 + 96 + j));
                a1_ij = _mm256_set1_ps(*(a + i * 48 + 144 + j));
                mul0 = _mm256_mul_ps(b_jk, a0_ij);
                mul1 = _mm256_mul_ps(b_jk, a1_ij);
                sum2 = _mm256_add_ps(sum2, mul0);
                sum3 = _mm256_add_ps(sum3, mul1);

                a0_ij = _mm256_set1_ps(*(a + i * 48 + 192 + j));
                a1_ij = _mm256_set1_ps(*(a + i * 48 + 240 + j));
                mul0 = _mm256_mul_ps(b_jk, a0_ij);
                mul1 = _mm256_mul_ps(b_jk, a1_ij);
                sum4 = _mm256_add_ps(sum4, mul0);
                sum5 = _mm256_add_ps(sum5, mul1);

                a0_ij = _mm256_set1_ps(*(a + i * 48 + 288 + j));
                a1_ij = _mm256_set1_ps(*(a + i * 48 + 336 + j));
                mul0 = _mm256_mul_ps(b_jk, a0_ij);
                mul1 = _mm256_mul_ps(b_jk, a1_ij);
                sum6 = _mm256_add_ps(sum6, mul0);
                sum7 = _mm256_add_ps(sum7, mul1);
            }
            _mm256_store_ps(c + i * 48 + k, sum0);
            _mm256_store_ps(c + i * 48 + 48 + k, sum1);
            _mm256_store_ps(c + i * 48 + 96 + k, sum2);
            _mm256_store_ps(c + i * 48 + 144 + k, sum3);
            _mm256_store_ps(c + i * 48 + 192 + k, sum4);
            _mm256_store_ps(c + i * 48 + 240 + k, sum5);
            _mm256_store_ps(c + i * 48 + 288 + k, sum6);
            _mm256_store_ps(c + i * 48 + 336 + k, sum7);
        }
    }
}

void sgemm_fma_48_48_48(float *a, float *b, float *c)
{
    for (size_t i = 0; i < 48; i += 2) {
        __m256 sum00 = _mm256_setzero_ps();
        __m256 sum01 = _mm256_setzero_ps();
        __m256 sum02 = _mm256_setzero_ps();
        __m256 sum03 = _mm256_setzero_ps();
        __m256 sum04 = _mm256_setzero_ps();
        __m256 sum05 = _mm256_setzero_ps();
        __m256 sum10 = _mm256_setzero_ps();
        __m256 sum11 = _mm256_setzero_ps();
        __m256 sum12 = _mm256_setzero_ps();
        __m256 sum13 = _mm256_setzero_ps();
        __m256 sum14 = _mm256_setzero_ps();
        __m256 sum15 = _mm256_setzero_ps();
        for (size_t j = 0; j < 48; ++j) {
            __m256 a0_ij = _mm256_set1_ps(*(a + i * 48 + j));
            __m256 a1_ij = _mm256_set1_ps(*(a + i * 48 + 48 + j));
            
            float* b_j = b + j * 48;
            
            __m256 bv_j;

            bv_j = _mm256_load_ps(b_j + 0);
            sum00 = _mm256_fmadd_ps(bv_j, a0_ij, sum00);
            sum10 = _mm256_fmadd_ps(bv_j, a1_ij, sum10);

            bv_j = _mm256_load_ps(b_j + 8);
            sum01 = _mm256_fmadd_ps(bv_j, a0_ij, sum01);
            sum11 = _mm256_fmadd_ps(bv_j, a1_ij, sum11);

            bv_j = _mm256_load_ps(b_j + 16);
            sum02 = _mm256_fmadd_ps(bv_j, a0_ij, sum02);
            sum12 = _mm256_fmadd_ps(bv_j, a1_ij, sum12);

            bv_j = _mm256_load_ps(b_j + 24);
            sum03 = _mm256_fmadd_ps(bv_j, a0_ij, sum03);
            sum13 = _mm256_fmadd_ps(bv_j, a1_ij, sum13);

            bv_j = _mm256_load_ps(b_j + 32);
            sum04 = _mm256_fmadd_ps(bv_j, a0_ij, sum04);
            sum14 = _mm256_fmadd_ps(bv_j, a1_ij, sum14);

            bv_j = _mm256_load_ps(b_j + 40);
            sum05 = _mm256_fmadd_ps(bv_j, a0_ij, sum05);
            sum15 = _mm256_fmadd_ps(bv_j, a1_ij, sum15);
        }

        _mm256_store_ps(c + i * 48 + 0, sum00);
        _mm256_store_ps(c + i * 48 + 8, sum01);
        _mm256_store_ps(c + i * 48 + 16, sum02);
        _mm256_store_ps(c + i * 48 + 24, sum03);
        _mm256_store_ps(c + i * 48 + 32, sum04);
        _mm256_store_ps(c + i * 48 + 40, sum05);
        _mm256_store_ps(c + i * 48 + 48 + 0, sum10);
        _mm256_store_ps(c + i * 48 + 48 + 8, sum11);
        _mm256_store_ps(c + i * 48 + 48 + 16, sum12);
        _mm256_store_ps(c + i * 48 + 48 + 24, sum13);
        _mm256_store_ps(c + i * 48 + 48 + 32, sum14);
        _mm256_store_ps(c + i * 48 + 48 + 40, sum15);
    }
}
