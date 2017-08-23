#ifndef SCANFFT_H
#define SCANFFT_H


#include <cstdint>


#include <ScanFFT_Config.h>
#include <ScanFFT_Utils.h>


#define SCANFFT_TRANSFORM(name1, name0, log2OfSize) \
    ScanFFT::Transform(name1##Reals, name1##Imags, name0##Reals, name0##Imags, log2OfSize)
#define SCANFFT_INVERSE_TRANSFORM(name1, name0, log2OfSize) \
    ScanFFT::InverseTransform(name1##Reals, name1##Imags, name0##Reals, name0##Imags, log2OfSize)


namespace ScanFFT {

void Setup(size_t log2OfMaxSize);
void Transform(Float *destReals, Float *destImags, Float const *srcReals, Float const *srcImags, uint8_t log2OfSize);
void InverseTransform(Float *destReals, Float *destImags, Float const *srcReals, Float const *srcImags, uint8_t log2OfSize);
void Cleanup();

}


#endif