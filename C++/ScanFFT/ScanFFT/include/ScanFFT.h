#ifndef SCANFFT_H
#define SCANFFT_H


#include <cstdint>


#include <ScanFFT_Config.h>
#include <ScanFFT_Utils.h>


namespace ScanFFT {

void Setup(size_t log2OfMaxSize);
void Transform(Float *destReals, Float *destImags, Float const *srcReals, Float const *srcImags, uint8_t log2OfSize);
void InverseTransform(Float *destReals, Float *destImags, Float const *srcReals, Float const *srcImags, uint8_t log2OfSize);
void Cleanup();

}


#endif