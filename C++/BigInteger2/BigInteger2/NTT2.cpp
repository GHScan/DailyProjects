
#include <algorithm>


#include "NTT2.h"
#include "MultiplePrecisionOp.h"


namespace RingArithmetic_NTT2 {

    // ring (mod M)
    // where M=2^(32*qn)+1, not a prime number

    inline size_t RingNumberSize(size_t qn) {
        return qn + 1;
    }

    inline size_t RingNumberSizeToQn(size_t numSize) {
        return numSize - 1;
    }

    static int Compare(uint32_t *digits0, uint32_t const *digits1, size_t size) {
        auto i = size;
        for (; i > 0 && digits0[i - 1] == digits1[i - 1]; --i);
        return i == 0 ? 0 : (digits0[i - 1] < digits1[i - 1] ? -1 : 1);
    }

    static void Add(uint32_t *digits0, uint32_t const *digits1, size_t size, uint32_t const *digitsM) {
        
        if (MultiplePrecisionOp::Add(digits0, size, digits1, size))
            ASSERT(0);

        if (digits0[size - 1] == 2) {
            digits0[size - 1] = 0;
            Memset(digits0, 0xff, size - 1);
        } else if (digits0[size - 1] == 1) {
            if (MultiplePrecisionOp::Substract(digits0, size, digitsM, size)) {
                digits0[size - 1] = 1;
                Memset(digits0, 0, size - 1);
            }
        } else {
            ASSERT(digits0[size - 1] == 0);
        }
    }

    static void Substract(uint32_t *digits0, uint32_t const *digits1, size_t size, uint32_t const *digitsM) {
        if (Compare(digits0, digits1, size) < 0) {
            if (MultiplePrecisionOp::Add(digits0, size, digitsM, size))
                ASSERT(0);
        } 
        if (MultiplePrecisionOp::Substract(digits0, size, digits1, size))
            ASSERT(0);
    }

    static void MultiplyTo(
        uint32_t *output, 
        uint32_t const *digits0, uint32_t const *digits1, size_t size, 
        uint32_t const *digitsM, uint32_t *buf) {

        Memset(buf, 0, size * 2);
        MultiplePrecisionOp::Multiply_NTT2(buf, size * 2, digits0, size, digits1, size);

        Memcpy(output, buf, size - 1);
        output[size - 1] = 0;
        Substract(output, buf + size - 1, size, digitsM);
    }

    static void LShiftTo(
        uint32_t *output, uint32_t const *digits, size_t size, size_t k, 
        uint32_t const *digitsM, uint32_t *buf) {
        
        ASSERT(k < size);

        Memset(buf + k + 1, 0, size - k - 1);
        Memcpy(buf, digits + (size - 1 - k), k + 1);

        Memset(output, 0, k);
        Memcpy(output + k, digits, size - 1 - k);
        output[size - 1] = 0;

        Substract(output, buf, size, digitsM);
    }

    static void RShiftTo(
        uint32_t *output, uint32_t const *digits, size_t size, size_t k,
        uint32_t const *digitsM, uint32_t *buf) {

        ASSERT(k < size);

        Memset(buf, 0, size - 1 - k);
        Memcpy(buf + (size - 1 - k), digits, k);
        buf[size - 1] = 0;

        Memset(output + size - k, 0, k);
        Memcpy(output, digits + k, size - k);

        Substract(output, buf, size, digitsM);
    }

    static void RShiftBitsTo(
        uint32_t *output, uint32_t const *digits, size_t size, size_t k,
        uint32_t const *digitsM, uint32_t *buf) {

        ASSERT(k < 32);

        Memset(buf, 0, size - 2);
        buf[size - 2] = digits[0] << (32 - k);
        buf[size - 1] = 0;

        uint32_t carry = digits[0] >> k;
        for (size_t i = 1; i < size; ++i) {
            output[i - 1] = digits[i] << (32 - k) | carry;
            carry = digits[i] >> k;
        }
        output[size - 1] = carry;

        Substract(output, buf, size, digitsM);
    }

    static std::vector<uint32_t> CreateModularDigits(size_t size) {
        std::vector<uint32_t> digits(size);
        digits.front() = 1;
        digits.back() = 1;
        return digits;
    }
}

using namespace RingArithmetic_NTT2;


extern void NumberTheoreticTransform2 (
    uint32_t *dest,
    uint32_t *src,
    size_t size,
    size_t ringNumberSize) {

    auto qn = RingNumberSizeToQn(ringNumberSize);

    ASSERT(IsPowerOf2(size));
    ASSERT(2 * qn % size == 0);

    std::vector<uint32_t> bufVec(ringNumberSize * 2), m = CreateModularDigits(ringNumberSize);
    auto buf0 = &bufVec[0], buf1 = &bufVec[ringNumberSize], digitsM = &m[0];

    for (size_t i = 0, bc = static_cast<size_t>(log2(size)); i < size; ++i)
        Memcpy(dest + ReverseBits(i, bc) * ringNumberSize, src + i * ringNumberSize, ringNumberSize);

    for (size_t s = 2; s <= size; s <<= 1) {
        size_t halfS = s / 2;
        size_t w = 2 * qn / s;
        for (size_t i = 0; i < size; i += s) {
            for (size_t j = 0, wi = 0; j < halfS; ++j, wi += w) {
                auto c0 = dest + (i + j) * ringNumberSize, c1 = dest + (i + j + halfS) * ringNumberSize;
                LShiftTo(buf0, c1, ringNumberSize, wi, digitsM, buf1);
                Memcpy(c1, c0, ringNumberSize);
                Add(c0, buf0, ringNumberSize, digitsM);
                Substract(c1, buf0, ringNumberSize, digitsM);
            }
        }
    }
}

extern void InverseNumberTheoreticTransform2(
    uint32_t *dest,
    uint32_t *src,
    size_t size,
    size_t ringNumberSize) {

    auto qn = RingNumberSizeToQn(ringNumberSize);

    ASSERT(IsPowerOf2(size));
    ASSERT(2 * qn % size == 0);

    std::vector<uint32_t> bufVec(ringNumberSize * 2), m = CreateModularDigits(ringNumberSize);
    auto buf0 = &bufVec[0], buf1 = &bufVec[ringNumberSize], digitsM = &m[0];

    for (size_t i = 0, bc = static_cast<size_t>(log2(size)); i < size; ++i) {
        RShiftBitsTo(buf0, src + i * ringNumberSize, ringNumberSize, bc, digitsM, buf1);
        Memcpy(dest + ReverseBits(i, bc) * ringNumberSize, buf0, ringNumberSize);
    }

    for (size_t s = 2; s <= size; s <<= 1) {
        size_t halfS = s / 2;
        size_t w = 2 * qn / s;
        for (size_t i = 0; i < size; i += s) {
            for (size_t j = 0, wi = 0; j < halfS; ++j, wi += w) {
                auto c0 = dest + (i + j) * ringNumberSize, c1 = dest + (i + j + halfS) * ringNumberSize;
                RShiftTo(buf0, c1, ringNumberSize, wi, digitsM, buf1);
                Memcpy(c1, c0, ringNumberSize);
                Add(c0, buf0, ringNumberSize, digitsM);
                Substract(c1, buf0, ringNumberSize, digitsM);
            }
        }
    }
}


extern bool EstimateNTT2NumberSize(
    size_t inputSize0, size_t inputSize1,
    size_t &rawNumberSize, size_t &ringNumberSize) {
    
    rawNumberSize = static_cast<size_t>(ceil(sqrt(NextPowerOf2(inputSize0 + inputSize1))));

    auto inputBlockCount0 = (inputSize0 + rawNumberSize - 1) / rawNumberSize;
    auto inputBlockCount1 = (inputSize1 + rawNumberSize - 1) / rawNumberSize;
    auto nttSize = NextPowerOf2(inputBlockCount0 + inputBlockCount1 - 1);
    auto halfNttSize = nttSize / 2;

    auto qn = static_cast<size_t>(ceil((2 * rawNumberSize + log2(nttSize) / 32) / halfNttSize)) * halfNttSize;
    ringNumberSize = RingNumberSize(qn);

    ASSERT(32 * qn >= 32 * rawNumberSize * 2 + log2(nttSize));

    return ringNumberSize != inputSize0 || ringNumberSize != inputSize1;
}


extern void Convolve_NTT2(
    uint32_t *output, size_t outputSize,
    uint32_t const *input0, size_t inputSize0,
    uint32_t const *input1, size_t inputSize1,
    size_t rawNumberSize, size_t ringNumberSize) {

    auto inputBlockCount0 = (inputSize0 + rawNumberSize - 1) / rawNumberSize;
    auto inputBlockCount1 = (inputSize1 + rawNumberSize - 1) / rawNumberSize;
    auto outputBlockCount = (outputSize + ringNumberSize - 1) / ringNumberSize;

    auto qn = RingNumberSizeToQn(ringNumberSize);
    auto nttSize = NextPowerOf2(inputBlockCount0 + inputBlockCount1 - 1);

    ASSERT(outputBlockCount >= inputBlockCount0 + inputBlockCount1 - 1);
    ASSERT(2 * qn % nttSize == 0);
    ASSERT(32 * qn >= 32 * rawNumberSize * 2 + log2(nttSize));
    ASSERT(ringNumberSize != inputSize0 || ringNumberSize != inputSize1); // otherwise we may encounter stack overflow


    std::vector<uint32_t> bufVec(ringNumberSize * nttSize * 3 + ringNumberSize * 2), m = CreateModularDigits(ringNumberSize);
    auto buf0 = &bufVec[0], buf1 = &bufVec[ringNumberSize * nttSize], buf2 = &bufVec[ringNumberSize * nttSize * 2];
    auto bufMul = &bufVec[ringNumberSize * nttSize * 3], digitsM = &m[0];



    for (size_t i = 0, io = 0; i < inputSize0; i += rawNumberSize, io += ringNumberSize)
        Memcpy(buf0 + io, input0 + i, std::min(rawNumberSize, inputSize0 - i));
    NumberTheoreticTransform2(buf1, buf0, nttSize, ringNumberSize);

    if (input0 == input1 && inputSize0 == inputSize1) {
        Memcpy(buf0, buf1, nttSize * ringNumberSize);
    } else {
        for (size_t i = 0, io = 0; i < inputSize1; i += rawNumberSize, io += ringNumberSize)
            Memcpy(buf2 + io, input1 + i, std::min(rawNumberSize, inputSize1 - i));
        NumberTheoreticTransform2(buf0, buf2, nttSize, ringNumberSize);
    }


    for (auto i = 0; i < nttSize; ++i) {
        MultiplyTo(
            buf2 + i * ringNumberSize, 
            buf0 + i * ringNumberSize, 
            buf1 + i * ringNumberSize, 
            ringNumberSize, digitsM, bufMul);
    }


    InverseNumberTheoreticTransform2(buf0, buf2, nttSize, ringNumberSize);
    for (auto i = 0; i < inputBlockCount0 + inputBlockCount1 - 1; ++i)
        Memcpy(output + i * ringNumberSize, buf0 + i * ringNumberSize, ringNumberSize);
}