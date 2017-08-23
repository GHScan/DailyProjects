#ifndef SCANFFTIMPL_DEFAULTFFT_H
#define SCANFFTIMPL_DEFAULTFFT_H


#include <cassert>


#include <vector>
#include <complex>


#include <ScanFFT_Utils.h>
#include <ScanFFT_ComplexV.h>
#include <ScanFFTImpl\BitReverseCopy.h>
#include <ScanFFTImpl\UnrolledFFT.h>


#define SCANFFT_DEFAULT_FFT(destReals, destImags, srcReals, srcImags, log2OfSize) do { \
    SCANFFT_CONCAT(ScanFFT::BitReverseCopy_, log2OfSize) (destReals, destImags, srcReals, srcImags); \
    ScanFFT::PostTransform(destReals, destImags, log2OfSize); \
} while (0)

#define SCANFFT_DEFAULT_IFFT(destReals, destImags, srcReals, srcImags, log2OfSize) do { \
    SCANFFT_CONCAT(ScanFFT::IBitReverseCopy_, log2OfSize) (destReals, destImags, srcReals, srcImags); \
    ScanFFT::IPostTransform(destReals, destImags, log2OfSize); \
} while (0)
#define SCANFFT_FALLBACK_FFT(destReals, destImags, srcReals, srcImags, log2OfSize) do { \
    ScanFFT::BitReverseCopy(destReals, destImags, srcReals, srcImags, log2OfSize); \
    ScanFFT::PostTransform(destReals, destImags, log2OfSize); \
} while (0)

#define SCANFFT_FALLBACK_IFFT(destReals, destImags, srcReals, srcImags, log2OfSize) do { \
    ScanFFT::IBitReverseCopy(destReals, destImags, srcReals, srcImags, log2OfSize); \
    ScanFFT::IPostTransform(destReals, destImags, log2OfSize); \
} while (0)


namespace ScanFFT {


static size_t ReverseBits(size_t i, size_t bits) {
    auto rev = (gReversedBytes[(i >> 0) & 0xff] << 24)
        | (gReversedBytes[(i >> 8) & 0xff] << 16)
        | (gReversedBytes[(i >> 16) & 0xff] << 8)
        | (gReversedBytes[(i >> 24) & 0xff] << 0);
    return rev >> (32 - bits);
}


static void BitReverseCopy(Float *destReals, Float *destImags, Float const *srcReals, Float const *srcImags, size_t log2OfSize) {
    auto size = 1ULL << log2OfSize;
    auto remainingBits = SCANFFT_UNROLLED_LOG2_OF_SIZE;
    assert(log2OfSize > remainingBits);
    auto reversedBits = log2OfSize - remainingBits;
    auto reversedBitsMask = (1ULL << reversedBits) - 1;
    for (size_t i = 0; i < size; ++i) {
        auto j = (i >> reversedBits) | (ReverseBits(i & reversedBitsMask, reversedBits) << remainingBits);
        destReals[j] = srcReals[i];
        destImags[j] = srcImags[i];
    }
}


static void IBitReverseCopy(Float *destReals, Float *destImags, Float const *srcReals, Float const *srcImags, size_t log2OfSize) {
    auto size = 1ULL << log2OfSize;
    auto scale = Float(1) / size;
    auto remainingBits = SCANFFT_UNROLLED_LOG2_OF_SIZE;
    assert(log2OfSize > remainingBits);
    auto reversedBits = log2OfSize - remainingBits;
    auto reversedBitsMask = (1ULL << reversedBits) - 1;
    for (size_t i = 0; i < size; ++i) {
        auto j = (i >> reversedBits) | (ReverseBits(i & reversedBitsMask, reversedBits) << remainingBits);
        destReals[j] = srcReals[i] * scale;
        destImags[j] = srcImags[i] * scale;
    }
}


static std::vector<Float*> gFactorRealMatrix, gFactorImagMatrix;
static std::vector<Float*> gFactor3RealMatrix, gFactor3ImagMatrix;
static std::vector<Float*> gIFactorRealMatrix, gIFactorImagMatrix;
static std::vector<Float*> gIFactor3RealMatrix, gIFactor3ImagMatrix;


static void SetupDefaultFFT(size_t log2OfMaxSize) {

    auto const kPi = Float(acos(-1));

    gFactorRealMatrix.resize(log2OfMaxSize + 1); gFactorImagMatrix.resize(log2OfMaxSize + 1); gFactor3RealMatrix.resize(log2OfMaxSize + 1); gFactor3ImagMatrix.resize(log2OfMaxSize + 1);
    gIFactorRealMatrix.resize(log2OfMaxSize + 1); gIFactorImagMatrix.resize(log2OfMaxSize + 1); gIFactor3RealMatrix.resize(log2OfMaxSize + 1); gIFactor3ImagMatrix.resize(log2OfMaxSize + 1);
    if (log2OfMaxSize >= SCANFFT_UNROLLED_LOG2_OF_SIZE + 1) {
        size_t log2OfSize = SCANFFT_UNROLLED_LOG2_OF_SIZE + 1;
        auto size = 1ULL << log2OfSize;
        auto halfSize = size >> 1;
        auto &factorReals = gFactorRealMatrix[log2OfSize] = Alloc<Float>(halfSize);
        auto &factorImags = gFactorImagMatrix[log2OfSize] = Alloc<Float>(halfSize);
        auto &ifactorReals = gIFactorRealMatrix[log2OfSize] = Alloc<Float>(halfSize);
        auto &ifactorImags = gIFactorImagMatrix[log2OfSize] = Alloc<Float>(halfSize);
        auto w = std::polar<Float>(1, 2 * kPi / size); auto wi = std::complex<Float>(1, 0);
        auto Iw = std::polar<Float>(1, -2 * kPi / size); auto Iwi = std::complex<Float>(1, 0);
        for (size_t i = 0; i < halfSize; ++i, wi *= w, Iwi *= Iw) {
            factorReals[i] = wi.real(); factorImags[i] = wi.imag();
            ifactorReals[i] = Iwi.real(); ifactorImags[i] = Iwi.imag();
        }
    }
    for (size_t log2OfSize = SCANFFT_UNROLLED_LOG2_OF_SIZE + 2; log2OfSize <= log2OfMaxSize; ++log2OfSize) {
        auto size = 1ULL << log2OfSize;
        auto quarterSize = size >> 2;
        auto &factorReals = gFactorRealMatrix[log2OfSize] = Alloc<Float>(quarterSize);
        auto &factorImags = gFactorImagMatrix[log2OfSize] = Alloc<Float>(quarterSize);
        auto &ifactorReals = gIFactorRealMatrix[log2OfSize] = Alloc<Float>(quarterSize);
        auto &ifactorImags = gIFactorImagMatrix[log2OfSize] = Alloc<Float>(quarterSize);
        auto &factor3Reals = gFactor3RealMatrix[log2OfSize] = Alloc<Float>(quarterSize);
        auto &factor3Imags = gFactor3ImagMatrix[log2OfSize] = Alloc<Float>(quarterSize);
        auto &ifactor3Reals = gIFactor3RealMatrix[log2OfSize] = Alloc<Float>(quarterSize);
        auto &ifactor3Imags = gIFactor3ImagMatrix[log2OfSize] = Alloc<Float>(quarterSize);

        auto w = std::polar<Float>(1, 2 * kPi / size); auto wi = std::complex<Float>(1, 0);
        auto Iw = std::polar<Float>(1, -2 * kPi / size); auto Iwi = std::complex<Float>(1, 0);
        for (size_t i = 0; i < quarterSize; ++i, wi *= w, Iwi *= Iw) {
            auto wi3 = wi * wi * wi; auto Iwi3 = Iwi * Iwi * Iwi;
            factorReals[i] = wi.real(); factorImags[i] = wi.imag();
            ifactorReals[i] = Iwi.real(); ifactorImags[i] = Iwi.imag();
            factor3Reals[i] = wi3.real(); factor3Imags[i] = wi3.imag();
            ifactor3Reals[i] = Iwi3.real(); ifactor3Imags[i] = Iwi3.imag();
        }
    }
}

static void CleanupDefaultFFT() {
    for (auto p : gFactorRealMatrix) Free(p); for (auto p : gFactorImagMatrix) Free(p);
    for (auto p : gFactor3RealMatrix) Free(p); for (auto p : gFactor3ImagMatrix) Free(p);
    for (auto p : gIFactorRealMatrix) Free(p); for (auto p : gIFactorImagMatrix) Free(p);
    for (auto p : gIFactor3RealMatrix) Free(p); for (auto p : gIFactor3ImagMatrix) Free(p);
    gFactorRealMatrix.clear(); gFactorImagMatrix.clear(); gFactor3RealMatrix.clear(); gFactor3ImagMatrix.clear();
    gIFactorRealMatrix.clear(); gIFactorImagMatrix.clear(); gIFactor3RealMatrix.clear(); gIFactor3ImagMatrix.clear();
}


static void PostTransform(Float *destReals, Float *destImags, uint8_t log2OfSize) {
    switch (log2OfSize) {
    case SCANFFT_UNROLLED_LOG2_OF_SIZE: 
        SCANFFT_UNROLLED_INPLACE_FFT(destReals, destImags, SCANFFT_UNROLLED_LOG2_OF_SIZE); 
        break;
    case SCANFFT_UNROLLED_LOG2_OF_SIZE + 1:
    {
        // Radix 2 FFT
        auto size = 1ULL << log2OfSize;
        auto halfSize = size >> 1;

        PostTransform(destReals, destImags, log2OfSize - 1);
        PostTransform(destReals + halfSize, destImags + halfSize, log2OfSize - 1);

        auto factorReals = &gFactorRealMatrix[log2OfSize][0], factorImags = &gFactorImagMatrix[log2OfSize][0];
        for (size_t i = 0; i < halfSize; i += SCANFFT_COMPLEXV_DIMENSION) {
            SCANFFT_COMPLEXV_LOAD(c1, dest, halfSize + i);
            SCANFFT_COMPLEXV_LOAD(wi, factor, i);
            SCANFFT_COMPLEXV_MUL(c1TimesWi, c1, wi);

            SCANFFT_COMPLEXV_LOAD(c0, dest, i);

            SCANFFT_COMPLEXV_ADD(x0, c0, c1TimesWi);
            SCANFFT_COMPLEXV_STORE(dest, i, x0);

            SCANFFT_COMPLEXV_SUB(x1, c0, c1TimesWi);
            SCANFFT_COMPLEXV_STORE(dest, halfSize + i, x1);
        }

        break;
    }
    default:
    {
        // Split Radix FFT 

        assert(log2OfSize > SCANFFT_UNROLLED_LOG2_OF_SIZE + 1);

        auto size = 1ULL << log2OfSize;
        auto halfSize = size >> 1;
        auto quarterSize = halfSize >> 1;

        PostTransform(destReals, destImags, log2OfSize - 1);
        PostTransform(destReals + halfSize, destImags + halfSize, log2OfSize - 2);
        PostTransform(destReals + halfSize + quarterSize, destImags + halfSize + quarterSize, log2OfSize - 2);

        auto factorReals = &gFactorRealMatrix[log2OfSize][0], factorImags = &gFactorImagMatrix[log2OfSize][0];
        auto factor3Reals = &gFactor3RealMatrix[log2OfSize][0], factor3Imags = &gFactor3ImagMatrix[log2OfSize][0];
        for (size_t i = 0; i < quarterSize; i += SCANFFT_COMPLEXV_DIMENSION) {
            SCANFFT_COMPLEXV_LOAD(c2, dest, halfSize + i);
            SCANFFT_COMPLEXV_LOAD(wi, factor, i);
            SCANFFT_COMPLEXV_MUL(c2TimesWi, c2, wi);

            SCANFFT_COMPLEXV_LOAD(c3, dest, halfSize + quarterSize + i);
            SCANFFT_COMPLEXV_LOAD(wi3, factor3, i);
            SCANFFT_COMPLEXV_MUL(c3TimesWi3, c3, wi3);
            {
                SCANFFT_COMPLEXV_ADD(c2TimesWiPlusC3TimesWi3, c2TimesWi, c3TimesWi3);
                SCANFFT_COMPLEXV_LOAD(c0, dest, i);

                SCANFFT_COMPLEXV_ADD(x0, c0, c2TimesWiPlusC3TimesWi3);
                SCANFFT_COMPLEXV_STORE(dest, i, x0);

                SCANFFT_COMPLEXV_SUB(x2, c0, c2TimesWiPlusC3TimesWi3);
                SCANFFT_COMPLEXV_STORE(dest, halfSize + i, x2);
            }
            {
                SCANFFT_COMPLEXV_SUB(c2TimesWiMinusC3TimesWi3, c2TimesWi, c3TimesWi3);
                SCANFFT_COMPLEXV_LOAD(c1, dest, quarterSize + i);

                SCANFFT_COMPLEXV_ADD_TIMES_I(x1, c1, c2TimesWiMinusC3TimesWi3);
                SCANFFT_COMPLEXV_STORE(dest, quarterSize + i, x1);

                SCANFFT_COMPLEXV_SUB_TIMES_I(x3, c1, c2TimesWiMinusC3TimesWi3);
                SCANFFT_COMPLEXV_STORE(dest, halfSize + quarterSize + i, x3);
            }
        }

        break;
    }
    }
}

static void IPostTransform(Float *destReals, Float *destImags, uint8_t log2OfSize) {
    switch (log2OfSize) {
    case SCANFFT_UNROLLED_LOG2_OF_SIZE: 
        SCANFFT_UNROLLED_INPLACE_IFFT(destReals, destImags, SCANFFT_UNROLLED_LOG2_OF_SIZE); 
        break;
    case SCANFFT_UNROLLED_LOG2_OF_SIZE + 1:
    {
        auto size = 1ULL << log2OfSize;
        auto halfSize = size >> 1;

        IPostTransform(destReals, destImags, log2OfSize - 1);
        IPostTransform(destReals + halfSize, destImags + halfSize, log2OfSize - 1);

        auto factorReals = &gIFactorRealMatrix[log2OfSize][0], factorImags = &gIFactorImagMatrix[log2OfSize][0];
        for (size_t i = 0; i < halfSize; i += SCANFFT_COMPLEXV_DIMENSION) {
            SCANFFT_COMPLEXV_LOAD(c1, dest, halfSize + i);
            SCANFFT_COMPLEXV_LOAD(wi, factor, i);
            SCANFFT_COMPLEXV_MUL(c1TimesWi, c1, wi);

            SCANFFT_COMPLEXV_LOAD(c0, dest, i);

            SCANFFT_COMPLEXV_ADD(x0, c0, c1TimesWi);
            SCANFFT_COMPLEXV_STORE(dest, i, x0);

            SCANFFT_COMPLEXV_SUB(x1, c0, c1TimesWi);
            SCANFFT_COMPLEXV_STORE(dest, halfSize + i, x1);
        }

        break;
    }
    default:
    {
        assert(log2OfSize > SCANFFT_UNROLLED_LOG2_OF_SIZE + 1);

        auto size = 1ULL << log2OfSize;
        auto halfSize = size >> 1;
        auto quarterSize = halfSize >> 1;

        IPostTransform(destReals, destImags, log2OfSize - 1);
        IPostTransform(destReals + halfSize, destImags + halfSize, log2OfSize - 2);
        IPostTransform(destReals + halfSize + quarterSize, destImags + halfSize + quarterSize, log2OfSize - 2);

        auto factorReals = &gIFactorRealMatrix[log2OfSize][0], factorImags = &gIFactorImagMatrix[log2OfSize][0];
        auto factor3Reals = &gIFactor3RealMatrix[log2OfSize][0], factor3Imags = &gIFactor3ImagMatrix[log2OfSize][0];
        for (size_t i = 0; i < quarterSize; i += SCANFFT_COMPLEXV_DIMENSION) {
            SCANFFT_COMPLEXV_LOAD(c2, dest, halfSize + i);
            SCANFFT_COMPLEXV_LOAD(wi, factor, i);
            SCANFFT_COMPLEXV_MUL(c2TimesWi, c2, wi);

            SCANFFT_COMPLEXV_LOAD(c3, dest, halfSize + quarterSize + i);
            SCANFFT_COMPLEXV_LOAD(wi3, factor3, i);
            SCANFFT_COMPLEXV_MUL(c3TimesWi3, c3, wi3);
            {
                SCANFFT_COMPLEXV_ADD(c2TimesWiPlusC3TimesWi3, c2TimesWi, c3TimesWi3);
                SCANFFT_COMPLEXV_LOAD(c0, dest, i);

                SCANFFT_COMPLEXV_ADD(x0, c0, c2TimesWiPlusC3TimesWi3);
                SCANFFT_COMPLEXV_STORE(dest, i, x0);

                SCANFFT_COMPLEXV_SUB(x2, c0, c2TimesWiPlusC3TimesWi3);
                SCANFFT_COMPLEXV_STORE(dest, halfSize + i, x2);
            }
            {
                SCANFFT_COMPLEXV_SUB(c2TimesWiMinusC3TimesWi3, c2TimesWi, c3TimesWi3);
                SCANFFT_COMPLEXV_LOAD(c1, dest, quarterSize + i);

                SCANFFT_COMPLEXV_ADD_TIMES_NEG_I(x1, c1, c2TimesWiMinusC3TimesWi3);
                SCANFFT_COMPLEXV_STORE(dest, quarterSize + i, x1);

                SCANFFT_COMPLEXV_SUB_TIMES_NEG_I(x3, c1, c2TimesWiMinusC3TimesWi3);
                SCANFFT_COMPLEXV_STORE(dest, halfSize + quarterSize + i, x3);
            }
        }

        break;
    }
    }
}


}

#endif