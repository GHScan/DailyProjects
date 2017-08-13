#include "stdafx.h"


#include "Utility.h"
#include "FastestFT.h"


#ifdef ENABLE_FASTEST_FT


#include "FFT_gen.h"


extern void FastestFourierTransform(
    std::complex<double> *dest,
    std::complex<double> const *src,
    size_t size) {

    ASSERT(IsPowerOf2(size));

    switch (size) {
    case 1: FFT_1(dest, src); break;
    case 2: FFT_2(dest, src); break;
    case 4: FFT_4(dest, src); break;
    case 8: FFT_8(dest, src); break;
    case 16: FFT_16(dest, src); break;
    case 32: FFT_32(dest, src); break;
    case 64: FFT_64(dest, src); break;
    case 128: FFT_128(dest, src); break;
    case 256: FFT_256(dest, src); break;
    case 512: FFT_512(dest, src); break;
    case 1024: FFT_1024(dest, src); break;
    case 2048: FFT_2048(dest, src); break;
    case 4096: FFT_4096(dest, src); break;
    case 8192: FFT_8192(dest, src); break;
    case 16384: FFT_16384(dest, src); break;
    case 32768: FFT_32768(dest, src); break;
    case 65536: FFT_65536(dest, src); break;
    case 131072: FFT_131072(dest, src); break;
    case 262144: FFT_262144(dest, src); break;
    case 524288: FFT_524288(dest, src); break;
    case 1048576: FFT_1048576(dest, src); break;
    default: ASSERT(0); break;
    }
}

extern void InverseFastestFourierTransform(
    std::complex<double> *dest,
    std::complex<double> const *src,
    size_t size) {
 
    ASSERT(IsPowerOf2(size));

    switch (size) {
    case 1: IFFT_1(dest, src); break;
    case 2: IFFT_2(dest, src); break;
    case 4: IFFT_4(dest, src); break;
    case 8: IFFT_8(dest, src); break;
    case 16: IFFT_16(dest, src); break;
    case 32: IFFT_32(dest, src); break;
    case 64: IFFT_64(dest, src); break;
    case 128: IFFT_128(dest, src); break;
    case 256: IFFT_256(dest, src); break;
    case 512: IFFT_512(dest, src); break;
    case 1024: IFFT_1024(dest, src); break;
    case 2048: IFFT_2048(dest, src); break;
    case 4096: IFFT_4096(dest, src); break;
    case 8192: IFFT_8192(dest, src); break;
    case 16384: IFFT_16384(dest, src); break;
    case 32768: IFFT_32768(dest, src); break;
    case 65536: IFFT_65536(dest, src); break;
    case 131072: IFFT_131072(dest, src); break;
    case 262144: IFFT_262144(dest, src); break;
    case 524288: IFFT_524288(dest, src); break;
    case 1048576: IFFT_1048576(dest, src); break;
    default: ASSERT(0); break;
    }
}

#endif