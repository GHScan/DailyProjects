#include "stdafx.h"


#include "NTT.h"


using namespace NTTFieldArithmetic;


static void NTTImpl(
    uint64_t *dest,
    uint64_t const *src,
    size_t size,
    uint64_t w,
    uint64_t scale) {

    std::vector<uint64_t> ws(size / 2);
    {
        uint64_t wi = 1;
        for (auto &v : ws) {
            v = wi;
            wi = Multiply(wi, w);
        }
    }

    if (scale == 1) {
        for (size_t i = 0, bc = BitCount(size) - 1; i < size; ++i)
            dest[BitReversal(i, bc)] = src[i];
    } else {
        for (size_t i = 0, bc = BitCount(size) - 1; i < size; ++i)
            dest[BitReversal(i, bc)] = Multiply(src[i], scale);
    }

    for (size_t s = 2; s <= size; s <<= 1) {
        size_t halfS = s / 2;
        size_t dw = size / s;
        for (size_t i = 0; i < size; i += s) {
            for (size_t j = 0, wi = 0; j < halfS; ++j, wi += dw) {
                auto c1 = dest[i + j], c2 = dest[i + j + halfS];
                auto wc2 = Multiply(c2, ws[wi]);
                dest[i + j] = Add(c1, wc2);
                dest[i + j + halfS] = Substract(c1, wc2);
            }
        }
    }
}


extern void NTT(
    uint64_t *dest,
    uint64_t const *src,
    size_t size) {
    
    ASSERT(IsPowerOf2(size));

    auto w = Power(kPrimitiveRoot, (kPrime - 1) / size);
    NTTImpl(dest, src, size, w, 1);
}

extern void InverseNTT(
    uint64_t *dest,
    uint64_t const *src,
    size_t size) {
 
    ASSERT(IsPowerOf2(size));

    auto w = Power(kPrimitiveRoot, kPrime - 1 - (kPrime - 1) / size);
    auto scale = Power(size, kPrime - 2);
    NTTImpl(dest, src, size, w, scale);
}