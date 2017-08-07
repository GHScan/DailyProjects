#ifndef NTT2_H
#define NTT2_H


#include <cstdint>


#include "Utility.h"


extern void NumberTheoreticTransform2(
    uint32_t *dest,
    uint32_t *src,
    size_t size,
    size_t ringNumberSize);

extern void InverseNumberTheoreticTransform2(
    uint32_t *dest,
    uint32_t *src,
    size_t size,
    size_t ringNumberSize);


extern bool EstimateNTT2NumberSize(
    size_t inputSize0, size_t inputSize1,
    size_t &rawNumberSize, size_t &ringNumberSize);


extern void Convolve_NTT2(
    uint32_t *output, size_t outputSize,
    uint32_t const *input0, size_t inputSize0,
    uint32_t const *input1, size_t inputSize1,
    size_t rawNumberSize, size_t ringNumberSize);


#endif