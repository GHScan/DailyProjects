#ifndef FASTEST_FT_H
#define FASTEST_FT_H


#include <complex>


// #define ENABLE_FASTEST_FT


extern void FastestFourierTransform(
    std::complex<double> *dest,
    std::complex<double> const *src,
    size_t size);
extern void InverseFastestFourierTransform(
    std::complex<double> *dest,
    std::complex<double> const *src,
    size_t size);


#endif