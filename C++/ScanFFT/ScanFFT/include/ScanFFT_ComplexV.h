#ifndef SCANFFT_COMPLEXV_H
#define SCANFFT_COMPLEXV_H


#include <ScanFFT_Config.h>


#if !SCANFFT_SIMD


#include <complex>


namespace ScanFFT {

template<typename FloatT>
inline std::complex<FloatT> make_complex(FloatT real, FloatT imag) {
    return std::complex<FloatT>(real, imag);
}

}


#define SCANFFT_COMPLEXV_DIMENSION 1
#define SCANFFT_COMPLEXV_ASSIGN(name1, name0) name1##Real = name0##Real, name1##Imag = name0##Imag
#define SCANFFT_COMPLEXV_MAKE1(name, real, imag) auto name = ScanFFT::make_complex(real, imag)
#define SCANFFT_COMPLEXV_LOAD(name, src, i) SCANFFT_COMPLEXV_MAKE1(name, *(src##Reals + i), *(src##Imags + i))
#define SCANFFT_COMPLEXV_STORE(dest, i, name) *(dest##Reals + i) = name.real(), *(dest##Imags + i) = name.imag()
#define SCANFFT_COMPLEXV_ADD(name2, name0, name1) auto name2 = name0 + name1
#define SCANFFT_COMPLEXV_ADD_TIMES_I(name2, name0, name1) SCANFFT_COMPLEXV_MAKE1(name2, name0.real() - name1.imag(), name0.imag() + name1.real())
#define SCANFFT_COMPLEXV_ADD_TIMES_NEG_I(name2, name0, name1) SCANFFT_COMPLEXV_MAKE1(name2, name0.real() + name1.imag(), name0.imag() - name1.real())
#define SCANFFT_COMPLEXV_SUB(name2, name0, name1) auto name2 = name0 - name1
#define SCANFFT_COMPLEXV_SUB_TIMES_I(name2, name0, name1) SCANFFT_COMPLEXV_ADD_TIMES_NEG_I(name2, name0, name1)
#define SCANFFT_COMPLEXV_SUB_TIMES_NEG_I(name2, name0, name1) SCANFFT_COMPLEXV_ADD_TIMES_I(name2, name0, name1)
#define SCANFFT_COMPLEXV_MUL(name2, name0, name1) auto name2 = name0 * name1


#endif


#if SCANFFT_SIMD && SCANFFT_SINGLE_PRECISION_FLOAT


#define SCANFFT_COMPLEXV_DIMENSION 8
#define SCANFFT_COMPLEXV_ASSIGN(name1, name0) name1##Real = name0##Real, name1##Imag = name0##Imag
#define SCANFFT_COMPLEXV_MAKE1(name, real, imag) auto name##Real = _mm256_set1_ps(real), name##Imag = _mm256_set1_ps(imag)
#define SCANFFT_COMPLEXV_LOAD(name, src, i) auto name##Real = _mm256_load_ps(src##Reals + i), name##Imag = _mm256_load_ps(src##Imags + i)
#define SCANFFT_COMPLEXV_STORE(dest, i, name) _mm256_store_ps(dest##Reals + i, name##Real), _mm256_store_ps(dest##Imags + i, name##Imag)
#define SCANFFT_COMPLEXV_ADD(name2, name0, name1) auto name2##Real = _mm256_add_ps(name0##Real, name1##Real), name2##Imag = _mm256_add_ps(name0##Imag, name1##Imag)
#define SCANFFT_COMPLEXV_ADD_TIMES_I(name2, name0, name1) auto name2##Real = _mm256_sub_ps(name0##Real, name1##Imag), name2##Imag = _mm256_add_ps(name0##Imag, name1##Real)
#define SCANFFT_COMPLEXV_ADD_TIMES_NEG_I(name2, name0, name1) auto name2##Real = _mm256_add_ps(name0##Real, name1##Imag), name2##Imag = _mm256_sub_ps(name0##Imag, name1##Real)
#define SCANFFT_COMPLEXV_SUB(name2, name0, name1) auto name2##Real = _mm256_sub_ps(name0##Real, name1##Real), name2##Imag = _mm256_sub_ps(name0##Imag, name1##Imag)
#define SCANFFT_COMPLEXV_SUB_TIMES_I(name2, name0, name1) SCANFFT_COMPLEXV_ADD_TIMES_NEG_I(name2, name0, name1)
#define SCANFFT_COMPLEXV_SUB_TIMES_NEG_I(name2, name0, name1) SCANFFT_COMPLEXV_ADD_TIMES_I(name2, name0, name1)
#define SCANFFT_COMPLEXV_MUL(name2, name0, name1) __m256 name2##Real, name2##Imag; do {\
    auto __realTimesReal = _mm256_mul_ps(name0##Real, name1##Real); \
    auto __realTimesImag = _mm256_mul_ps(name0##Real, name1##Imag); \
    auto __imagTimesReal = _mm256_mul_ps(name0##Imag, name1##Real); \
    auto __imagTimesImag = _mm256_mul_ps(name0##Imag, name1##Imag); \
    name2##Real = _mm256_sub_ps(__realTimesReal, __imagTimesImag); \
    name2##Imag = _mm256_add_ps(__realTimesImag, __imagTimesReal); \
} while(0)


#endif


#if SCANFFT_SIMD && !SCANFFT_SINGLE_PRECISION_FLOAT


#define SCANFFT_COMPLEXV_DIMENSION 4
#define SCANFFT_COMPLEXV_ASSIGN(name1, name0) name1##Real = name0##Real, name1##Imag = name0##Imag
#define SCANFFT_COMPLEXV_MAKE1(name, real, imag) auto name##Real = _mm256_set1_pd(real), name##Imag = _mm256_set1_pd(imag)
#define SCANFFT_COMPLEXV_LOAD(name, src, i) auto name##Real = _mm256_load_pd(src##Reals + i), name##Imag = _mm256_load_pd(src##Imags + i)
#define SCANFFT_COMPLEXV_STORE(dest, i, name) _mm256_store_pd(dest##Reals + i, name##Real), _mm256_store_pd(dest##Imags + i, name##Imag)
#define SCANFFT_COMPLEXV_ADD(name2, name0, name1) auto name2##Real = _mm256_add_pd(name0##Real, name1##Real), name2##Imag = _mm256_add_pd(name0##Imag, name1##Imag)
#define SCANFFT_COMPLEXV_ADD_TIMES_I(name2, name0, name1) auto name2##Real = _mm256_sub_pd(name0##Real, name1##Imag), name2##Imag = _mm256_add_pd(name0##Imag, name1##Real)
#define SCANFFT_COMPLEXV_ADD_TIMES_NEG_I(name2, name0, name1) auto name2##Real = _mm256_add_pd(name0##Real, name1##Imag), name2##Imag = _mm256_sub_pd(name0##Imag, name1##Real)
#define SCANFFT_COMPLEXV_SUB(name2, name0, name1) auto name2##Real = _mm256_sub_pd(name0##Real, name1##Real), name2##Imag = _mm256_sub_pd(name0##Imag, name1##Imag)
#define SCANFFT_COMPLEXV_SUB_TIMES_I(name2, name0, name1) SCANFFT_COMPLEXV_ADD_TIMES_NEG_I(name2, name0, name1)
#define SCANFFT_COMPLEXV_SUB_TIMES_NEG_I(name2, name0, name1) SCANFFT_COMPLEXV_ADD_TIMES_I(name2, name0, name1)
#define SCANFFT_COMPLEXV_MUL(name2, name0, name1) __m256d name2##Real, name2##Imag; do {\
    auto __realTimesReal = _mm256_mul_pd(name0##Real, name1##Real); \
    auto __realTimesImag = _mm256_mul_pd(name0##Real, name1##Imag); \
    auto __imagTimesReal = _mm256_mul_pd(name0##Imag, name1##Real); \
    auto __imagTimesImag = _mm256_mul_pd(name0##Imag, name1##Imag); \
    name2##Real = _mm256_sub_pd(__realTimesReal, __imagTimesImag); \
    name2##Imag = _mm256_add_pd(__realTimesImag, __imagTimesReal); \
} while(0)


#endif


#endif