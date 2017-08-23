#ifndef SCANFFTIMPL_UNROLLEDFFT_H
#define SCANFFTIMPL_UNROLLEDFFT_H


#include <ScanFFT_Config.h>


#define SCANFFT_UNROLLED_LOG2_OF_SIZE  8
#define SCANFFT_UNROLLED_FFT(destReals, destImags, srcReals, srcImags, log2OfSize)  SCANFFT_CONCAT(ScanFFT::FFT_, log2OfSize) (destReals, destImags, srcReals, srcImags)
#define SCANFFT_UNROLLED_IFFT(destReals, destImags, srcReals, srcImags, log2OfSize)  SCANFFT_CONCAT(ScanFFT::IFFT_, log2OfSize) (destReals, destImags, srcReals, srcImags)
#define SCANFFT_UNROLLED_INPLACE_FFT(destReals, destImags, log2OfSize)  SCANFFT_CONCAT(ScanFFT::InplaceFFT_, log2OfSize) (destReals, destImags)
#define SCANFFT_UNROLLED_INPLACE_IFFT(destReals, destImags, log2OfSize)  SCANFFT_CONCAT(ScanFFT::InplaceIFFT_, log2OfSize) (destReals, destImags)



#if !SCANFFT_SIMD 
#include <tuple>
using std::make_tuple;
using std::get;
#else
#include <immintrin.h>
#include <emmintrin.h>
#endif

namespace ScanFFT {

#if !SCANFFT_SIMD && SCANFFT_SINGLE_PRECISION_FLOAT
#include <ScanFFTImpl\Generated\UnrolledFFT_Single.h>
#elif !SCANFFT_SIMD && !SCANFFT_SINGLE_PRECISION_FLOAT
#include <ScanFFTImpl\Generated\UnrolledFFT_Double.h>
#elif SCANFFT_SIMD && SCANFFT_SINGLE_PRECISION_FLOAT
#include <ScanFFTImpl\Generated\UnrolledFFT_SingleSIMD.h>
#elif SCANFFT_SIMD && !SCANFFT_SINGLE_PRECISION_FLOAT
#include <ScanFFTImpl\Generated\UnrolledFFT_DoubleSIMD.h>
#endif

}


#endif