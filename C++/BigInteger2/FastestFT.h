#ifndef FASTEST_FT_H
#define FASTEST_FT_H


#include <complex>

#include "Utility.h"


#define ENABLE_FASTEST_FT 0


extern void FastestFourierTransform(
    std::complex<FFTFloat> *dest,
    std::complex<FFTFloat> const *src,
    size_t size);
extern void InverseFastestFourierTransform(
    std::complex<FFTFloat> *dest,
    std::complex<FFTFloat> const *src,
    size_t size);


#endif