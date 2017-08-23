#include "ScanFFT.h"
#include "ScanFFTImpl\UnrolledFFT.h"
#include "ScanFFTImpl\DefaultFFT.h"


namespace ScanFFT {


void Setup(size_t log2OfMaxSize) {
    SetupDefaultFFT(log2OfMaxSize);
}


void Cleanup() {
    CleanupDefaultFFT();
}


void Transform(Float *destReals, Float *destImags, Float const *srcReals, Float const *srcImags, uint8_t log2OfSize) {
#define CASE_UNROLLED_FFT(i)  case i: SCANFFT_UNROLLED_FFT(destReals, destImags, srcReals, srcImags, i); break
#define CASE_DEFAULT_FFT(i)  case i: SCANFFT_DEFAULT_FFT(destReals, destImags, srcReals, srcImags, i); break
#define CASE_FALLBACK_FFT() default: SCANFFT_FALLBACK_FFT(destReals, destImags, srcReals, srcImags, log2OfSize); break

    switch (log2OfSize) {
        CASE_UNROLLED_FFT(0);
        CASE_UNROLLED_FFT(1);
        CASE_UNROLLED_FFT(2);
        CASE_UNROLLED_FFT(3);
        CASE_UNROLLED_FFT(4);
        CASE_UNROLLED_FFT(5);
        CASE_UNROLLED_FFT(6);
        CASE_UNROLLED_FFT(7);
        CASE_UNROLLED_FFT(8);
        CASE_DEFAULT_FFT(9);
        CASE_DEFAULT_FFT(10);
        CASE_DEFAULT_FFT(11);
        CASE_DEFAULT_FFT(12);
        CASE_DEFAULT_FFT(13);
        CASE_DEFAULT_FFT(14);
        CASE_DEFAULT_FFT(15);
        CASE_DEFAULT_FFT(16);
        CASE_DEFAULT_FFT(17);
        CASE_DEFAULT_FFT(18);
        CASE_DEFAULT_FFT(19);
        CASE_DEFAULT_FFT(20);
        CASE_DEFAULT_FFT(21);
        CASE_DEFAULT_FFT(22);
        CASE_DEFAULT_FFT(23);
        CASE_DEFAULT_FFT(24);
        CASE_FALLBACK_FFT();
    }

#undef CASE_FALLBACK_FFT
#undef CASE_DEFAULT_FFT
#undef CASE_UNROLLED_FFT
}


void InverseTransform(Float *destReals, Float *destImags, Float const *srcReals, Float const *srcImags, uint8_t log2OfSize) {
#define CASE_UNROLLED_IFFT(i)  case i: SCANFFT_UNROLLED_IFFT(destReals, destImags, srcReals, srcImags, i); break
#define CASE_DEFAULT_IFFT(i)  case i: SCANFFT_DEFAULT_IFFT(destReals, destImags, srcReals, srcImags, i); break
#define CASE_FALLBACK_IFFT() default: SCANFFT_FALLBACK_IFFT(destReals, destImags, srcReals, srcImags, log2OfSize); break

    switch (log2OfSize) {
        CASE_UNROLLED_IFFT(0);
        CASE_UNROLLED_IFFT(1);
        CASE_UNROLLED_IFFT(2);
        CASE_UNROLLED_IFFT(3);
        CASE_UNROLLED_IFFT(4);
        CASE_UNROLLED_IFFT(5);
        CASE_UNROLLED_IFFT(6);
        CASE_UNROLLED_IFFT(7);
        CASE_UNROLLED_IFFT(8);
        CASE_DEFAULT_IFFT(9);
        CASE_DEFAULT_IFFT(10);
        CASE_DEFAULT_IFFT(11);
        CASE_DEFAULT_IFFT(12);
        CASE_DEFAULT_IFFT(13);
        CASE_DEFAULT_IFFT(14);
        CASE_DEFAULT_IFFT(15);
        CASE_DEFAULT_IFFT(16);
        CASE_DEFAULT_IFFT(17);
        CASE_DEFAULT_IFFT(18);
        CASE_DEFAULT_IFFT(19);
        CASE_DEFAULT_IFFT(20);
        CASE_DEFAULT_IFFT(21);
        CASE_DEFAULT_IFFT(22);
        CASE_DEFAULT_IFFT(23);
        CASE_DEFAULT_IFFT(24);
        CASE_FALLBACK_IFFT();
    }

#undef CASE_FALLBACK_IFFT
#undef CASE_DEFAULT_IFFT
#undef CASE_UNROLLED_IFFT
}


}