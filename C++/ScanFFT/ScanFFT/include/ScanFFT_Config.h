#ifndef SCANFFT_CONFIG_H
#define SCANFFT_CONFIG_H


#define SCANFFT_SINGLE_PRECISION_FLOAT 0
#define SCANFFT_SIMD 1


namespace ScanFFT {


#if SCANFFT_SINGLE_PRECISION_FLOAT
using Float = float;
#else
using Float = double;
#endif


}


#endif